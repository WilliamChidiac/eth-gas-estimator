open Eth
open Utilities
open Common_types
open Constant

exception Tx_type_undefined of string

type 'a tx_pool = {
  pending : (transaction * int) list ref;
  blacklist : (address, bint * int) Hashtbl.t;
  mutex_pending : Mutex.t;
  mutex_account : Mutex.t;
  current_base_fee : float ref;
}

let empty =
  {
    pending = ref [];
    blacklist = Hashtbl.create 10;
    mutex_pending = Mutex.create ();
    mutex_account = Mutex.create ();
    current_base_fee = ref 0.;
  }

(**sorted lists*)

let mempool = empty

(**calculate the priority fee of a transaction at a certain time.
  for each type of transaction (0, 1, 2) there is a different method to calculate the priority fee.
  however type 0 and type 1 uses both the same technic (but more research should be done on type 1 )
    note that the returned value should not be stored since it varies depending on the base fees.*)
let calc_priority_fee ?(bf = !(mempool.current_base_fee)) tx =
  match tx.typ with
  | None -> 0.
  | Some 0 | Some 1 -> bz_to_float tx.gas_price -. !(mempool.current_base_fee)
  | Some 2 ->
    let a_mpfpg = bzOption_to_float tx.max_priority_fee_per_gas in
    let a_mpfg = bzOption_to_float tx.max_fee_per_gas in
    min a_mpfpg (a_mpfg -. !(mempool.current_base_fee))
  | Some x ->
    raise
      (Tx_type_undefined (Printf.sprintf "Unknown type of transaction : %d" x))

let compare_by_priority_fee ?(bf = !(mempool.current_base_fee)) a b =
  compare (calc_priority_fee ~bf b) (calc_priority_fee ~bf a)

let sort_by comp mempool =
  List.sort (fun a b -> comp (fst a) (fst b)) !(mempool.pending)

let sort_by_priority_fee ?(bf = !(mempool.current_base_fee)) mempool =
  sort_by (compare_by_priority_fee ~bf) mempool

(**add tx in sroted structure by Priority fee*)
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
  mempool.pending := aux !(mempool.pending) ;
  Mutex.unlock mempool.mutex_pending

let process_pending_tx tx mempool =
  let tx_gp = bz_to_float tx.gas_price in
  try
    let nonce, age = Hashtbl.find mempool.blacklist tx.from in
    if tx.tx_nonce < nonce then (
      Mutex.lock mempool.mutex_account ;
      Hashtbl.replace mempool.blacklist tx.from (tx.tx_nonce, age) ;
      Mutex.unlock mempool.mutex_account
    )
  with Not_found ->
    if tx_gp < min_gp then (
      Mutex.lock mempool.mutex_account ;
      Hashtbl.add mempool.blacklist tx.from (tx.tx_nonce, 0) ;
      Mutex.unlock mempool.mutex_account ;
      Mutex.lock mempool.mutex_pending ;
      mempool.pending :=
        List.filter
          (fun pair ->
            if (fst pair).from <> tx.from then
              true
            else
              false)
          !(mempool.pending) ;
      Mutex.unlock mempool.mutex_pending
    ) else
      add_tx tx mempool

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
  mempool.pending := rm_tx tx !(mempool.pending) ;
  Mutex.unlock mempool.mutex_pending

let update_pending_age mempool =
  let rec aux = function
    | [] -> []
    | e :: sub_l ->
      if snd e >= delta_pending_tx then
        aux sub_l
      else
        (fst e, snd e + 1) :: aux sub_l in
  mempool.pending := aux !(mempool.pending)

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

let update_base_fee mempool (bf : baseFee) =
  let base = float_of_string bf.base_fee in
  let gas = float_of_string bf.gas_used in
  mempool.current_base_fee :=
    (base +. (base *. ((gas -. estimate) /. estimate *. max_coeff)))
    /. 1000000000.

let update_mempool mempool (bf : baseFee) =
  update_base_fee mempool bf ;
  update_pending_age mempool ;
  update_blacklist_age mempool
