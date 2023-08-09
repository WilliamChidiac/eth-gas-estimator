open Eth
open Utilities
open Common_types
open Constant

type tx_pool = {
  mutable pending : (transaction * int) list;
  blacklist : (address, transaction list * int) Hashtbl.t;
  mutex_pending : Mutex.t;
  mutex_account : Mutex.t;
  mutable current_base_fee : Z.t;
  mutable pool_min_pf : Z.t;
}

let empty =
  {
    pending = [];
    blacklist = Hashtbl.create 10;
    mutex_pending = Mutex.create ();
    mutex_account = Mutex.create ();
    current_base_fee = Z.of_int 0;
    pool_min_pf = min_gp;
  }

(**sorted lists*)

let mempool = empty

(**[calc_priority_fee tx] returns the current priority fee of a transaction [tx] since it chages overtime depending on the base fee.
  the priority fee's formula depends on the type of the transaction [tx] :
   for [tx] of type 0 and 1
    priority fee = gas price - base fee
  for [tx] of type 2 
    priority fee = min (max fee per gas - base fee , max priority fee)*)
let calc_priority_fee ?(bf = mempool.current_base_fee) = calc_priority_fee bf

(**[compare_by_priority_fee a b] helps comparing 2 transaction for sorting and filtering purposes*)
let compare_by_priority_fee a b =
  compare (calc_priority_fee b) (calc_priority_fee a)

(**[sort_by_priority_fee list] sorts a list of (transaction, transaction_age) in a decreasing order of the priority fees*)
let sort_by_priority_fee list =
  let sort_by comp list = List.sort (fun a b -> comp (fst a) (fst b)) list in
  sort_by compare_by_priority_fee list

(**[add_tx tx mempool] add a pending transaction [tx] in the [mempool],
    if another transaction has the same nonce as [tx.tx_nonce] and sender address [tx.from] is already in the mempool
      only one of them will still be in the mempool at the end of the process, 
    the one with the greater priority fee.*)
let add_tx tx mempool =
  let rec aux l =
    match l with
    | [] -> [(tx, 0)]
    | pair :: sub_l ->
      let t = fst pair in
      if t.from = tx.from && t.tx_nonce = tx.tx_nonce then
        if calc_priority_fee t < calc_priority_fee tx then
          (tx, 0) :: sub_l
        else
          l
      else
        pair :: aux sub_l in
  Mutex.lock mempool.mutex_pending ;
  mempool.pending <- aux mempool.pending ;
  Mutex.unlock mempool.mutex_pending

(**[process_pending_tx tx mempool] helps filter the transaction before adding them to the mempool. 
    it takes a pending transaction [tx] and a tx_pool [mempool]
    and adds the transaction to the mempool if it has valid inputs 
    or blacklist the sender's address if it is not already blacklisted.*)
let process_pending_tx tx mempool =
  let tx_pf = calc_priority_fee tx in
  try
    let nonce, age = Hashtbl.find mempool.blacklist tx.from in
    if tx.tx_nonce < nonce then (
      Mutex.lock mempool.mutex_account ;
      Hashtbl.replace mempool.blacklist tx.from (tx.tx_nonce, age) ;
      Mutex.unlock mempool.mutex_account
    )
  with Not_found ->
    if tx_pf < mempool.pool_min_pf then (
      Mutex.lock mempool.mutex_account ;
      Hashtbl.add mempool.blacklist tx.from (tx.tx_nonce, 0) ;
      Mutex.unlock mempool.mutex_account ;
      Mutex.lock mempool.mutex_pending ;
      mempool.pending <-
        List.filter
          (fun pair ->
            if (fst pair).from <> tx.from then
              true
            else
              false)
          mempool.pending ;
      Mutex.unlock mempool.mutex_pending
    ) else
      add_tx tx mempool

(**[remove_tx tx mempool] used to remove a mined transaction from the mempool*)
let remove_tx tx mempool =
  let rec rm_tx tx l =
    match l with
    | [] -> []
    | e :: subl ->
      if (fst e).tx_nonce = tx.tx_nonce && (fst e).from = tx.from then
        rm_tx tx subl
      else
        e :: rm_tx tx subl in
  Mutex.lock mempool.mutex_pending ;
  mempool.pending <- rm_tx tx mempool.pending ;
  Mutex.unlock mempool.mutex_pending

(**[update_pending_age mempool] updates the age of the stored pending transaction
    if a transaction has reached it's maximum age, it is considered as invalid and 
    is removed from the mempool.*)
let update_pending_age mempool =
  let rec aux = function
    | [] -> []
    | e :: sub_l ->
      if snd e >= delta_pending_tx then
        aux sub_l
      else
        (fst e, snd e + 1) :: aux sub_l in
  mempool.pending <- aux mempool.pending

(**[update_blacklist_age mempool] updates the age of the blacklisted addresses. 
    if an adressed has reached it's maximum aged, it is removed from the blacklist*)
let update_blacklist_age mempool =
  let aux_tbl = Hashtbl.copy mempool.blacklist in
  Mutex.lock mempool.mutex_account ;
  Hashtbl.iter
    (fun key value ->
      if snd value >= delta_account then
        Hashtbl.remove mempool.blacklist key
      else
        Hashtbl.replace mempool.blacklist key (fst value, snd value + 1))
    aux_tbl ;
  Mutex.unlock mempool.mutex_account

(**[update_base_fee mempool bf] calculates the new base fee using last block informations
    and stores it in mempool.current_base_fee.*)
let update_base_fee mempool (bf : block_header) =
  mempool.current_base_fee <- Utilities.newBaseFee bf ;
  mempool.pool_min_gp <- Utilities.min_gas_price mempool.current_base_fee

(**[update_mempool mempool bf] wrapper of all the update functions. 
    this function should be called every time a new block is published*)
let update_mempool mempool (bf : block_header) =
  update_base_fee mempool bf ;
  update_pending_age mempool ;
  update_blacklist_age mempool
