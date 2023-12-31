open Eth
open Common_types
open Utilities

type pool = Sorted_list.tx_pool

type local_stats = {
  mutable sts_block_number : bint; [@key "block_number"]
  mutable sts_base_fee : bz; [@key "base_fee"]
  mutable sts_gas_used : bint; [@key "gas"]
  mutable sts_block_length : bint; [@key "number_of_tx"]
  mutable sts_intersection : bint; [@key "tx_block_mempool"]
  mutable sts_compare : (transaction * bool) list; [@key "block_transaction"]
  mutable sts_is_mev : bool; [@key "mev_boost"]
}
[@@deriving encoding]

type overall_stats = {
  mutable mev_intersection : bz; [@key "mev_intersection_pr"]
  mutable mev_total : bz; [@key "mev_intersection_number"]
  mutable normal_intersection : bz; [@key "mev_intersection_pr"]
  mutable normal_total : bz; [@key "mev_intersection_number"]
}
[@@deriving encoding]

type snapshot = {
  mutable id : int; [@key "id"]
  mempools : (bint, transaction list) Hashtbl.t; [@key "mempool"]
  stats : (bint, local_stats) Hashtbl.t; [@key "block_stats"]
  mutable latest_block : (transaction * bool) list; [@key "latest_accecpted"]
  mutex_latest_block : Mutex.t;
}

let block_builders = Constant.block_bs

let init_stats () =
  {
    sts_block_number = 0;
    sts_base_fee = Z.of_int 0;
    sts_gas_used = 0;
    sts_block_length = 0;
    sts_intersection = 0;
    sts_compare = [];
    sts_is_mev = false;
  }

let snap_shot =
  {
    id = 0;
    mempools = Hashtbl.create Constant.lifespan.delta_snapshot;
    stats = Hashtbl.create Constant.lifespan.delta_snapshot;
    latest_block = [];
    mutex_latest_block = Mutex.create ();
  }

let global =
  {
    mev_intersection = Z.of_int 0;
    mev_total = Z.of_int 0;
    normal_intersection = Z.of_int 0;
    normal_total = Z.of_int 0;
  }

let is_block_builder header =
  try
    List.find (fun builder -> builder = header.fee_recepient) !block_builders
    |> fun _ -> true
  with Not_found -> false

let snapshot_mined tx in_mempool =
  Mutex.lock snap_shot.mutex_latest_block ;
  snap_shot.latest_block <- (tx, in_mempool) :: snap_shot.latest_block ;
  Mutex.unlock snap_shot.mutex_latest_block

let snapshot_stats_block_header bh =
  let sts = Hashtbl.find_opt snap_shot.stats snap_shot.id in
  match sts with
  | None -> ()
  | Some stats ->
    stats.sts_is_mev <- is_block_builder bh ;
    stats.sts_block_number <- bh.number ;
    stats.sts_base_fee <- Z.of_string bh.base_fee ;
    stats.sts_gas_used <- int_of_string bh.gas_used

let snapshot_block bh =
  snap_shot.id <- bh.number ;
  let new_stats = init_stats () in
  Hashtbl.add snap_shot.stats snap_shot.id new_stats ;
  snapshot_stats_block_header bh ;
  Mutex.lock snap_shot.mutex_latest_block ;
  let rm_txs = List.map (fun x -> x) snap_shot.latest_block in
  Mutex.unlock snap_shot.mutex_latest_block ;
  match Hashtbl.find_opt snap_shot.stats (snap_shot.id - 1) with
  | None -> ()
  | Some stats ->
    let rec update_stats = function
      | [] -> (stats.sts_intersection, stats.sts_block_length)
      | (_, is_in) :: sub ->
        if is_in then stats.sts_intersection <- stats.sts_intersection + 1 ;
        stats.sts_block_length <- stats.sts_block_length + 1 ;
        update_stats sub in
    stats.sts_compare <- rm_txs ;
    let inter, total = update_stats snap_shot.latest_block in
    if stats.sts_is_mev then (
      global.mev_total <- Z.(global.mev_total + Z.of_int total) ;
      global.mev_intersection <- Z.(global.mev_intersection + Z.of_int inter)
    ) else (
      global.normal_total <- Z.(global.normal_total + Z.of_int total) ;
      global.normal_intersection <-
        Z.(global.normal_intersection + Z.of_int inter)
    ) ;
    snap_shot.latest_block <- []

let snapshot_mempool pool =
  Hashtbl.add snap_shot.mempools snap_shot.id
    (List.fold_left (fun a (tx, _age) -> tx :: a) [] pool)

let remove_snapshot key =
  let keys = Hashtbl.to_seq_keys snap_shot.stats in
  match Seq.find (fun b_num -> b_num < key) keys with
  | None -> ()
  | Some x ->
    Hashtbl.remove snap_shot.mempools x ;
    Hashtbl.remove snap_shot.stats x

let print_local_stats ?(sort = true) stats =
  match stats with
  | None -> Format.eprintf "no stats for this block@."
  | Some stats ->
    let compare_list =
      if sort then
        List.fast_sort
          (fun (t1, _) (t2, _) ->
            Utilities.compare_by_priority_fee stats.sts_base_fee t1 t2)
          stats.sts_compare
      else
        stats.sts_compare in
    List.iter
      (fun (tx, b) ->
        Format.eprintf
          (if b then
             "|destination   |   \\ /     | %s | \n\
              |priority fees |    X      | %s |       \n\
              |transaction_id|   / \\     |    %d    | \n\
              --------------------------------------------------------------------------------------------------@."
           else
             "|destination   |           | %s \n\
              |priority fees |           |    %s \n\
              |transaction_id|           |    %d \n\
              --------------------------------------------------------------------------------------------------@.")
          (EzEncoding.construct b_enc tx.tx_hash)
          (print_in (calc_priority_fee stats.sts_base_fee tx))
          (Option.value ~default:(-1) tx.transaction_index))
      compare_list ;
    Format.eprintf
      "block number = %d \n\
       base fee = %s \n\
       gas used = %d \n\
       tx in mempool and block = %d \n\
       block length = %d \n\
       %% of tx in block that you had over number of tx in block = %.2f %%\n\n\
       overall stats : \n\
       block builder = %b \n\n\
       mev boost builder stats : \n\
       intersection = %.2f %% \n\
       normal builder stats : \n\
       intersection = %.2f %% \n\
       @."
      stats.sts_block_number
      (print_in stats.sts_base_fee)
      stats.sts_gas_used stats.sts_intersection stats.sts_block_length
      (float_of_int stats.sts_intersection
      /. float_of_int stats.sts_block_length
      *. 100.)
      stats.sts_is_mev
      (Z.to_float global.mev_intersection /. Z.to_float global.mev_total *. 100.)
      (Z.to_float global.normal_intersection
      /. Z.to_float global.normal_total
      *. 100.)

let print_stats key = Hashtbl.find_opt snap_shot.stats key |> print_local_stats

let filter_stats () =
  if snap_shot.id > 1 then print_stats (snap_shot.id - 1) ;
  remove_snapshot (snap_shot.id - Constant.lifespan.delta_snapshot) ;
  ()

let sort_transactions bf list =
  List.fast_sort (fun a b -> compare_by_priority_fee bf a b) list

let estimate_priority podium (mempool : pool) =
  let bf = mempool.current_base_fee in
  let pending_l =
    List.fast_sort
      (fun (e1, _) (e2, _) -> compare_by_priority_fee bf e1 e2)
      mempool.pending in
  let mev_builder_in =
    Z.to_float global.mev_intersection /. Z.to_float global.mev_total in
  Format.eprintf "mev_builder = %.2f@." mev_builder_in ;
  let rec calc_tests pending_list gu index =
    match pending_list with
    | [] -> Z.of_int 10_000_000
    | (e, _lifespan) :: l_aux ->
      if gu > podium then
        let x = calc_priority_fee bf e in
        if x > Z.of_int 0 then
          x
        else
          Z.of_int 10_000_000
      else
        let fuel =
          if e.gas < Constant.max_gu then
            e.gas
          else
            Constant.max_gu in
        calc_tests l_aux (gu + fuel) (index + 1) in
  calc_tests pending_l 0 0
