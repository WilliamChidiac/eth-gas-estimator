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
  blocks : (bint, transaction list) Hashtbl.t; [@key "accepted"]
  block_headers : (bint, block_header) Hashtbl.t; [@key "block_header"]
  stats : (bint, local_stats) Hashtbl.t; [@key "block_stats"]
  mutable latest_block : transaction list; [@key "latest_accecpted"]
}

let block_builders =
  ref
    [
      "0x1f9090aae28b8a3dceadf281b0f12828e676c326";
      "0x388c818ca8b9251b393131c08a736a67ccb19297";
      "0x95222290dd7278aa3ddd389cc1e1d165cc4bafe5";
      "0x690b9a9e9aa1c9db991c7721a92d351db4fac990";
      "0xdafea492d9c6733ae3d56b7ed1adb60692c98bc5";
    ]

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
    blocks = Hashtbl.create Constant.lifespan.delta_snapshot;
    block_headers = Hashtbl.create Constant.lifespan.delta_snapshot;
    stats = Hashtbl.create Constant.lifespan.delta_snapshot;
    latest_block = [];
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

let erase_block () = snap_shot.latest_block <- []

let snapshot_mined tx = snap_shot.latest_block <- tx :: snap_shot.latest_block

let snapshot_stats_comp () =
  let accepted = snap_shot.latest_block in
  let pending = Hashtbl.find_opt snap_shot.mempools snap_shot.id in
  match pending with
  | None -> Format.eprintf "no pending transactions yet@."
  | Some pending -> (
    let stats = Hashtbl.find_opt snap_shot.stats snap_shot.id in
    match stats with
    | None -> Format.eprintf "no stats variable yet@."
    | Some stats ->
      let rec aux a =
        match a with
        | [] -> []
        | e :: l ->
          (if List.exists (fun p -> p.tx_hash = e.tx_hash) pending then
             (e, true)
           else
             (e, false))
          :: aux l in
      stats.sts_compare <- aux accepted)

let snapshot_block () =
  Hashtbl.add snap_shot.blocks snap_shot.id snap_shot.latest_block ;
  snapshot_stats_comp ()

let snapshot_stats_block bh =
  let sts = Hashtbl.find_opt snap_shot.stats snap_shot.id in
  match sts with
  | None -> ()
  | Some stats ->
    stats.sts_is_mev <- is_block_builder bh ;
    stats.sts_block_number <- bh.number ;
    stats.sts_base_fee <- Z.of_string bh.base_fee ;
    stats.sts_gas_used <- int_of_string bh.gas_used ;
    stats.sts_intersection <- !Sorted_list.inter ;
    stats.sts_block_length <- !Sorted_list.total ;
    if stats.sts_is_mev then (
      global.mev_total <- Z.(global.mev_total + Z.of_int !Sorted_list.total) ;
      global.mev_intersection <-
        Z.(global.mev_intersection + Z.of_int !Sorted_list.inter)
    ) else (
      global.normal_total <-
        Z.(global.normal_total + Z.of_int !Sorted_list.total) ;
      global.normal_intersection <-
        Z.(global.normal_intersection + Z.of_int !Sorted_list.inter)
    )

let snapshot_header bh =
  Hashtbl.add snap_shot.block_headers snap_shot.id bh ;
  snapshot_stats_block bh

let incr_id () =
  snap_shot.id <- (Hashtbl.find snap_shot.block_headers snap_shot.id).number + 1

let snapshot_mempool pool =
  Hashtbl.add snap_shot.mempools snap_shot.id
    (List.fold_left (fun a (tx, _life) -> tx :: a) [] pool)

let snapshot_state pool =
  let stats = init_stats () in
  snapshot_block () ;
  incr_id () ;
  snapshot_mempool pool ;
  Hashtbl.add snap_shot.stats snap_shot.id stats

let remove_snapshot key =
  let keys = Hashtbl.to_seq_keys snap_shot.stats in
  match Seq.find (fun b_num -> b_num < key) keys with
  | None -> ()
  | Some x ->
    Hashtbl.remove snap_shot.block_headers x ;
    Hashtbl.remove snap_shot.blocks x ;
    Hashtbl.remove snap_shot.mempools x ;
    Hashtbl.remove snap_shot.stats x

let print_stats key =
  let stats = Hashtbl.find_opt snap_shot.stats key in
  match stats with
  | None -> ()
  | Some stats ->
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
      stats.sts_compare ;
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
       snapshot id = %d \n\
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
      key

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
