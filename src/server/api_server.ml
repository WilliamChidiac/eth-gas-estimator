open Common_types

type estimation = { priority_fee : Eth.bz } [@@deriving encoding]

let nth_gas = EzAPI.Param.int ~name:"nth" "nth"

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

let get_lifespan _req () = EzAPIServer.return_ok Constant.lifespan
[@@get
  { path = "/get_lifespans"; output = lifespans_enc; name = "get_lifespans" }]

let post_update_lifespan _req updated_lifespan =
  let match_input input to_change =
    match (to_change, input) with
    | 0, Some x -> Constant.lifespan.delta_snapshot <- x
    | 1, Some x -> Constant.lifespan.delta_account <- x
    | 2, Some x -> Constant.lifespan.delta_snapshot <- x
    | _, _ -> () in
  match_input updated_lifespan.snapshots 2 ;
  match_input updated_lifespan.pending_txs 0 ;
  match_input updated_lifespan.blacklist 1 ;
  EzAPIServer.return_ok "changes applied"
[@@post
  {
    path = "/change_snapshot_storage";
    output = Json_encoding.string;
    input = poste_lifespan_enc;
    name = "change_snapshot_storage";
  }]

let init_server () =
  EzAPIServer.set_verbose 3 ;
  EzLwtSys.run @@ fun () ->
  let port = 8080 in
  EzAPIServer.(server [(port, API ppx_dir)])
