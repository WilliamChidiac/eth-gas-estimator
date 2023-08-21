open Lwt.Infix

let cmd_alchemy ~verbosity _options =
  Utilities.log ~verbosity "Starting alchemy command@." ;
  Lwt.async (fun _ ->
      Alchemy_request.request () >>= fun _ -> Lwt_unix.sleep 1000.) ;
  Api_server.init_server () ;
  Lwt.return_unit
