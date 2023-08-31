let main conf =
  let open Common_types in
  let verbosity = Utilities.set_verbose conf.verbose in
  Constant.url := Option.value ~default:!Constant.url conf.opt_node_url ;
  Constant.lifespan.delta_snapshot <- conf.delta_snapshot ;
  Constant.lifespan.delta_account <- conf.delta_account ;
  Constant.lifespan.delta_pending_tx <- conf.delta_transaction ;
  Constant.snapshot_delay := conf.delay_snapshot ;
  (match conf.mev_builders with
  | None -> ()
  | Some x -> Constant.block_bs := x) ;
  Utilities.log ~verbosity:Trace "Main.main > verbose: %d@." conf.verbose ;
  match conf.command with
  | Vanilla vanilla_options ->
    Utilities.log ~verbosity:Common_types.Trace "Main.main > Vanilla@." ;
    Cmd_vanilla.cmd_vanilla ~verbosity vanilla_options
  | Alchemy alchemy_options ->
    Utilities.log ~verbosity:Common_types.Trace "Main.main > Alchemy@." ;
    Cmd_alchemy.cmd_alchemy ~verbosity alchemy_options

let () =
  let open Common_types in
  parse_config () |> main |> Lwt_main.run
