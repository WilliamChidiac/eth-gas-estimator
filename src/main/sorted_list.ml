open Eth
open Utilities
open Common_types

type 'a sorted_structure = 'a list

let empty : transaction sorted_structure = []

(**sorted lists*)
let l_pfpg = ref empty

let l_average = ref empty

(**print function*)
let print_container l = List.iter (fun x -> print_trans x) l

let rec print_x_first l x =
  match (x, l) with
  | -1, _ | _, [] -> ()
  | _, e :: v ->
    print_trans e ;
    print_x_first v (x - 1)

(**just for testing*)
let total_removed_tx = ref 0

let total_accepted_tx = ref 0

(**used in the process of adding and removing elements from the sorted_structure*)
let mutex = Mutex.create ()

(**calculate the priority fee of a transaction at a certain time.
  for each type of transaction (0, 1, 2) there is a different method to calculate the priority fee.
  however type 0 and type 1 uses both the same technic (but more research should be done on type 1 )
    note that the returned value should not be stored since it varies depending on the base fees.*)
let calc_priority_fee tx =
  match tx.typ with
  | None -> 0.
  | Some 0 | Some 1 -> bz_to_float tx.gas_price -. !current_base_fee
  | Some 2 ->
    let a_mpfpg = bzOption_to_float tx.max_priority_fee_per_gas in
    let a_mpfg = bzOption_to_float tx.max_fee_per_gas in
    min a_mpfpg (a_mpfg -. !current_base_fee)
  | Some _ ->
    assert false (* TODO William, ton pattern matching n'était pas exhaustif *)

let compare_by_priority_fee a b =
  compare (calc_priority_fee b) (calc_priority_fee a)

let sort_by comp = List.sort comp !l_pfpg

(**generic function to add transaction in sorted_structure using a customized comparaing method*)
let add_tx_by compare_func tx_list tx =
  let rec add_pending_tx_aux tx_list tx =
    match tx_list with
    | t :: tl ->
      if compare_func t tx < 0 then
        t :: add_pending_tx_aux tl tx
      else
        tx :: tx_list
    | [] -> [tx] in
  add_pending_tx_aux tx_list tx

(**add tx in sroted structure by Priority fee*)
let add_tx_by_PFPG tx list =
  Mutex.lock mutex ;
  list := add_tx_by compare_by_priority_fee !list tx ;
  Mutex.unlock mutex

let add_tx tx list = add_tx_by_PFPG tx list

let minPF = ref 10.

let get_size () = List.length !l_pfpg

let remove_tx (tx : transaction) =
  total_accepted_tx := !total_accepted_tx + 1 ;
  let rec rm_tx tx l i =
    match l with
    | [] -> []
    | e :: subl ->
      if e.tx_hash = tx.tx_hash then (
        Format.eprintf "index of removed tx : %d@." i ;
        total_removed_tx := !total_removed_tx + 1 ;
        subl
      ) else
        e :: rm_tx tx subl (i + 1) in
  let pf =
    bzOption_to_float ~def:(Z.of_float !minPF) tx.max_priority_fee_per_gas in
  if pf > 0.00001 && !minPF > pf then
    minPF := pf
  else
    () ;
  Mutex.lock mutex ;
  l_pfpg := rm_tx tx !l_pfpg 0 ;
  Mutex.unlock mutex
