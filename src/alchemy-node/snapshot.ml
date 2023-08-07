open Eth
open Common_types
open Utilities

let snap_shot_id : int ref = ref 0

let snap_shot_pending : (bint, transaction list) Hashtbl.t = Hashtbl.create 10

let snap_shot_block : (bint, transaction list) Hashtbl.t = Hashtbl.create 10

let snap_shot_block_header : (bint, baseFee) Hashtbl.t = Hashtbl.create 10

let latest_validated_txs : transaction list ref = ref []

let snapshot_mined tx = latest_validated_txs := tx :: !latest_validated_txs

let erase_block () = latest_validated_txs := []

let snapshot_block () =
  Hashtbl.add snap_shot_block !snap_shot_id !latest_validated_txs

let snapshot_header b = Hashtbl.add snap_shot_block_header !snap_shot_id b

let incr_id () = snap_shot_id := 1 + !snap_shot_id

let snapshot_mempool mempool =
  let l = ref [] in
  List.iter (fun (tx, _nonce) -> l := tx :: !l) mempool ;
  Hashtbl.add snap_shot_pending !snap_shot_id !l

let snapshot_state mempool =
  snapshot_block () ;
  incr_id () ;
  snapshot_mempool mempool

let compare_by_priority_fee bf a b =
  let x = compare (calc_priority_fee bf b) (calc_priority_fee bf a) in
  if x = 0 then
    compare b.tx_hash a.tx_hash
  else
    x

let sort_by comp list = List.sort (fun a b -> comp a b) list

let sort_by_priority_fee bf list = sort_by (compare_by_priority_fee bf) list

(**[print_compare by_index key] shows statistics related to the mempool and a specific block at the moment it was released to help comparision with our estimations. 
  the comparision is shown sorted by transaction index when by_priority is set to false,
  and sort by priority fee when the by_priority is to tru. 
  key is the nth validated block since the begining of the program. *)
let print_compare ?(by_priority = true) key =
  let pending = Hashtbl.find snap_shot_pending key in
  let accepted = Hashtbl.find snap_shot_block key in
  let block_header = Hashtbl.find snap_shot_block_header key in
  let block_number = block_header.number in
  let bf = Z.of_string block_header.base_fee in

  let pending_priority = sort_by_priority_fee bf pending in
  let accepted_priority = sort_by_priority_fee bf accepted in

  let intersection = ref 0 in
  let block_not_mempool = ref 0 in
  let accepted_tx_index =
    List.sort
      (fun a b -> compare a.transaction_index b.transaction_index)
      accepted in

  let rec print_priority pending accepted index_pending index_accept =
    match (pending, accepted) with
    | [], _ | _, [] -> block_not_mempool := index_accept - !intersection
    | pending_tx :: sub_pending, accepted_tx :: sub_accepted ->
      let p_pf = calc_priority_fee bf pending_tx in
      let a_pf = calc_priority_fee bf accepted_tx in
      if accepted_tx.tx_hash = pending_tx.tx_hash then (
        Format.eprintf
          "|index in list |    %d    |    %d    | \n\
           |priority fees | %s | %s | \n\
           |transaction id|          |    %d    | \n\
           --------------------------------------------------------------------------------------------------@."
          index_pending index_accept (print_in p_pf) (print_in a_pf)
          (Option.value ~default:(-1) accepted_tx.transaction_index) ;
        intersection := !intersection + 1 ;
        print_priority sub_pending sub_accepted (index_pending + 1)
          (index_accept + 1)
      ) else if compare_by_priority_fee bf pending_tx accepted_tx < 0 then (
        Format.eprintf
          "|index in list |    %d    |           | \n\
           |priority fees | %s |           | \n\
           |hash          | %s | \n\
           --------------------------------------------------------------------------------------------------@."
          index_pending (print_in p_pf)
          (EzEncoding.construct b_enc pending_tx.tx_hash) ;
        print_priority sub_pending accepted (index_pending + 1) index_accept
      ) else (
        Format.eprintf
          "|index in list |          |    %d    | \n\
           |priority fees |          | %s | \n\
           |transaction_id|          |    %d    | \n\
           --------------------------------------------------------------------------------------------------@."
          index_accept (print_in a_pf)
          (Option.value ~default:(-1) accepted_tx.transaction_index) ;
        print_priority pending sub_accepted index_pending (index_accept + 1)
      ) in
  let rec print_tx_index accepted =
    match accepted with
    | [] -> block_not_mempool := !block_not_mempool - !intersection
    | tx :: sub_accepted ->
      block_not_mempool := Option.value ~default:0 tx.transaction_index ;
      (let a_pf = calc_priority_fee bf tx in
       try
         let _ = List.find (fun t -> t.tx_hash = tx.tx_hash) pending_priority in
         Format.eprintf
           "|destination   |   \\ /     | %s | \n\
            |priority fees |    X      | %s |       \n\
            |transaction_id|   / \\     |    %d    | \n\
            --------------------------------------------------------------------------------------------------@."
           (EzEncoding.construct b_enc tx.tx_hash)
           (print_in a_pf)
           (Option.value ~default:(-1) tx.transaction_index) ;
         intersection := !intersection + 1
       with Not_found ->
         Format.eprintf
           "|destination   |           | %s \n\
            |priority fees |           |    %s \n\
            |transaction_id|           |    %d \n\
            --------------------------------------------------------------------------------------------------@."
           (EzEncoding.construct b_enc tx.tx_hash)
           (print_in a_pf)
           (Option.value ~default:(-1) tx.transaction_index)) ;
      print_tx_index sub_accepted in

  let calc_estimation podium pending_l =
    let rec calc_tests pending_list gu index =
      match pending_list with
      | [] -> Format.eprintf "nombre total de gas: %d\n@." gu
      | e :: l_aux ->
        if gu > podium then
          let x = calc_priority_fee bf e in
          Format.eprintf
            "top %d de gas: \nestimation = %s\nindex of estimation = %d\n@."
            podium (print_in x) index
        else
          let fuel =
            if e.gas < Constant.max_gu then
              e.gas
            else
              Constant.max_gu in
          calc_tests l_aux (gu + fuel) (index + 1) in
    calc_tests pending_l 0 0 in

  Format.eprintf
    "block number = %d \n\
     base fee = %s \n\
     gas used = %d \n\
     |              |Pending txs|accepted txs| @." block_number (print_in bf)
    (int_of_string block_header.gas_used) ;
  if by_priority = true then
    print_priority pending_priority accepted_priority 0 0
  else
    print_tx_index accepted_tx_index ;
  Format.eprintf
    "block number = %d \n\
     base fee = %s \n\
     gas used = %d \n\
     mempool length = %d \n\
     tx in mempool and block = %d \n\
     tx in block but not in mempool = %d \n\
     @."
    block_number (print_in bf)
    (int_of_string block_header.gas_used)
    (List.length pending) !intersection !block_not_mempool ;
  calc_estimation 10000000 pending_priority ;
  calc_estimation 15000000 pending_priority ;
  calc_estimation 20000000 pending_priority ;
  calc_estimation 25000000 pending_priority

let remove_snapshot key =
  Hashtbl.remove snap_shot_block key ;
  Hashtbl.remove snap_shot_block_header key ;
  Hashtbl.remove snap_shot_pending key

let by_priority = true

let by_tx_index = false

(**[print_stats compare] works exactly like [print_compare] except it always prints the latest snapshot and delets all previous snapshots*)
let print_stats ?(compare = by_priority) () =
  if !snap_shot_id > 1 then (
    print_compare (!snap_shot_id - 1) ~by_priority:compare ;
    remove_snapshot (!snap_shot_id - 2)
  ) else
    ()
