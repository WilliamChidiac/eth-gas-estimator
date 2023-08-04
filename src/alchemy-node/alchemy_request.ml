open Lwt.Infix
open Common_types
open Sorted_list
open Utilities
open Constant

(*websocket url*)
let uri = "wss://eth-mainnet.g.alchemy.com/v2/ytvWudRFU7i34JtwGZqu9MAynm_sUhK1"

(* alchemy_minedTransactions :  return all the transaction in a newBlock
   alchemy_pendingTransactions : return all the pending transaction in the mempool
   newHeads : return all the newly added blocks
*)

(*request messages*)

let alchemy_subscription meth =
  {
    req_rpc_version = "2.0";
    rq_id = 1;
    rq_method = "eth_subscribe";
    rq_params = [meth];
  }
  |> EzEncoding.construct requests_params_enc

let request_pending = alchemy_subscription "alchemy_pendingTransactions"

let request_mined = alchemy_subscription "alchemy_minedTransactions"

let request_new_block = alchemy_subscription "newHeads"

(**[react websocket  message] takes a json string as message, decodes it and treats it depending the type
    the json string can be either:
     - connection complete message
     - pending transaction 
     - mined trnasaction 
     - new block header 
     /*)
let react _ s =
  Lwt.dont_wait
    (fun () ->
      Lwt.return (response_from_json s) >>= fun resp ->
      match resp with
      | Connection _ ->
        Format.eprintf "Connection complete.@." ;
        Lwt.return () (*connected messages*)
      | Data d -> (
        match d with
        | Pending_transaction t ->
          Sorted_list.process_pending_tx t Sorted_list.mempool ;
          Lwt.return ()
        | Mined_transaction t ->
          Snapshot.snapshot_mined t.tx_tx ;
          Sorted_list.remove_tx t.tx_tx Sorted_list.mempool ;
          Lwt.return ()
        | Base_fee b ->
          Snapshot.erase_block () ;
          Snapshot.snapshot_header b ;
          Sorted_list.update_mempool Sorted_list.mempool b ;
          let time = Unix.gettimeofday () -. Int64.to_float b.timestamp in
          Lwt_unix.sleep (12. -. time) >>= fun _ ->
          Snapshot.snapshot_state Sorted_list.mempool.pending ;
          Snapshot.print_stats () ;
          Lwt.return ()))
    (fun exn -> Format.eprintf "exn:%s\n\n@." (Printexc.to_string exn)) ;
  Lwt.return (Ok ())

let error _ _ = failwith "error"

(**[refresh period] is an infite loop function that calls it self every [period] seconds,
    to make the program run forever*)
let rec refresh period =
  if true then
    Lwt_unix.sleep period >>= fun _ -> refresh period
  else
    Lwt.return (Ok ())

(**[request ()] connects to the websocket,
     send all the wanted subscription 
     then calls the refresh function to loop infinitly*)

let request () =
  begin
    Printf.printf "Connecting to %s\n%!" uri ;
    EzWs.connect ~error ~react uri >>= function
    | Error e ->
      prerr_endline e ;
      Lwt.return (Ok ())
    | Ok ws ->
      ws.action.send request_pending >>= fun _ ->
      ws.action.send request_mined >>= fun _ ->
      ws.action.send request_new_block >>= fun _ -> refresh !refresh_rate
  end
