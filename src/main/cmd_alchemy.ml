open Lwt.Infix

let cmd_alchemy ~verbosity _options =
  Utilities.log ~verbosity "Starting alchemy command@." ;
  Alchemy_request.request () >>= fun _ -> Lwt.return ()
