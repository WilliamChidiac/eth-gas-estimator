open Common_types

type estimation = { priority_fee : Eth.bz } [@@deriving encoding]

let nth_gas = EzAPI.Param.int ~name:"nth" "nth"

(**[/estimate_priority_fee?nth=[int]] or [/estimate_priority_fee]
    GET request that takes one optional parameter [nth:int] 
    [nth] represent the position in which the user wants his transaction to be placed in the next block "in terms of gas".
        the default value of [nth] is 15000000*)
let get_priority_fee_estimate req () =
  let podium = EzAPI.Req.find_param nth_gas req in
  let estimate =
    Snapshot.estimate_priority
      (match podium with
      | None -> 15_000_000
      | Some nth_gas -> int_of_string nth_gas)
      Sorted_list.mempool in
  Format.eprintf "the estimation is : %.2f@."
    (Q.to_float Q.(Q.of_bigint estimate / Q.of_int 1_000_000_000)) ;
  EzAPIServer.return_ok { priority_fee = estimate }
[@@get
  {
    path = "/estimate_priority_fee";
    output = estimation_enc;
    params = [nth_gas];
    name = "estimate_priority_fee";
  }]

type poste_lifespan = {
  snapshots : int option; [@key "snapshots_lifespan"]
  pending_txs : int option; [@key "transactions_lifespan"]
  blacklist : int option; [@key "blacklist_lifespan"]
}
[@@deriving encoding]

(**[/get_lifespans] 
    GET request that returns the number of block it takes for a
    [pending transaction] / [blacklisted account] / [snapshot] before it is deleted*)
let get_lifespan _req () = EzAPIServer.return_ok Constant.lifespan
[@@get
  { path = "/get_lifespans"; output = lifespans_enc; name = "get_lifespans" }]

(**[date [json:poste_lifespan] /change_snapshot_storage]
    POST function that changes the value of the lifespan variable in the constant file. 
    [snapshots_lifespan] changes the maximum age of a snapshot (in number of blocks) before in is deleted.
    [transactions_lifespan] changes the maximum age of a pending transaction in the mempool (in number of blocks) before it is removed/forgotten.
    [blacklist_lifespan] changes the maximum age of a blacklisted account (in number of blocks) before it is reconsidered trustworthy.
    Note that you can change one of the field at a time or all at once. 
    *)
let post_update_lifespan _req updated_lifespan =
  let match_input input to_change =
    match (to_change, input) with
    | 0, Some x -> Constant.lifespan.delta_pending_tx <- x
    | 1, Some x -> Constant.lifespan.delta_account <- x
    | 2, Some x -> Constant.lifespan.delta_snapshot <- x
    | _, _ -> () in
  match_input updated_lifespan.pending_txs 0 ;
  match_input updated_lifespan.blacklist 1 ;
  match_input updated_lifespan.snapshots 2 ;
  EzAPIServer.return_ok "changes applied"
[@@post
  {
    path = "/change_snapshot_storage";
    output = Json_encoding.string;
    input = poste_lifespan_enc;
    name = "change_snapshot_storage";
  }]

type post_delay = { new_delay : float [@key "delay"] } [@@deriving encoding]

(**[date [json:post_delay] /change_snapshot_delay]
    POST function that changes the value of the delay after a new block is added to take a snapshot of the mempool. 
    the type of the field [delay] is a [float]. *)
let post_update_snapshot_delay _req updated_delay =
  Constant.snapshot_delay := updated_delay.new_delay ;
  EzAPIServer.return_ok "changes applied"
[@@post
  {
    path = "/change_snapshot_delay";
    output = Json_encoding.string;
    input = post_delay_enc;
    name = "change_snapshot_delay";
  }]

let block_number = EzAPI.Param.int ~name:"block_number" "block_number"

type no_stats = { error_message : string [@key "error"] } [@@deriving encoding]

type stats_resp =
  | Valid of Snapshot.local_stats
  | Error of no_stats
[@@deriving encoding]

(**[/get_stats_of_block?block_number=[int]]
    GET request that takes a required a paramter [block_number].
    [returns] a json of some stats captured during the snapshot dedicated to this block. 
    if no paramter is passed or if a invalid parameter is passed, returns an error message.
    *)
let get_stats req () =
  let id = EzAPI.Req.find_param block_number req in
  EzAPIServer.return_ok
    (match id with
    | None ->
      Error { error_message = "required parameter missing: block_number" }
    | Some x -> (
      let x = int_of_string x in
      match Hashtbl.find_opt Snapshot.snap_shot.stats x with
      | None ->
        if x < Snapshot.snap_shot.id then
          Error
            {
              error_message =
                "this block is to old to be in scope of the program";
            }
        else
          Error { error_message = "this block has not been captured yet." }
      | Some stats -> Valid stats))
[@@get
  {
    path = "/get_stats_of_block";
    output = stats_resp_enc;
    params = [block_number];
    name = "get_stats_of_block";
  }]

(**[/get_overall_stats] 
  GET request that returns a [json] of global stats accumulated since the beginning of the program*)
let get_global_stats _req () = EzAPIServer.return_ok Snapshot.global
[@@get
  {
    path = "/get_overall_stats";
    output = Snapshot.overall_stats_enc;
    params = [];
    name = "get_overall_stats";
  }]

let init_server () =
  EzAPIServer.set_verbose 3 ;
  EzLwtSys.run @@ fun () ->
  let port = 8080 in
  EzAPIServer.(server [(port, API ppx_dir)])
