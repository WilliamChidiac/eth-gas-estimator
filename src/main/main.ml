let main conf =
  let open Common_types in
  let verbosity = Utilities.set_verbose conf.verbose in
  Utilities.log ~verbosity:Trace "Main.main > verbose: %d@." conf.verbose ;
  match conf.command with
  | Vanilla vanilla_options ->
    Utilities.log ~verbosity:Common_types.Trace "Main.main > Vanilla@." ;
    Cmd_vanilla.cmd_vanilla ~verbosity vanilla_options
  | Alchemy alchemy_options ->
    (* let token = Gitlab.Token.AccessToken "glpat--u6mxQg_uuWiyEX3p-DM" in *)
    Utilities.log ~verbosity:Common_types.Trace "Main.main > Alchemy@." ;
    Cmd_alchemy.cmd_alchemy ~verbosity alchemy_options

let () =
  let open Common_types in
  parse_config () |> main |> Lwt_main.run
