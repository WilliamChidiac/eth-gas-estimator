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

(**this function will be called every time we receive a notification. the receiveed message will then be treated depending on it's type.*)
let react (_w : string EzWs.action) s =
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
          Snapshot.snapshot_state !(Sorted_list.mempool.pending) ;
          Lwt.return ()))
    (fun exn -> Format.eprintf "exn:%s\n\n@." (Printexc.to_string exn)) ;
  Lwt.return (Ok ())

let error _ _ = failwith "error"

(**this function will be called every "period" seconds where period is a float.
  it uses the list of pending to calculate the probabilities of inclusion in the new block depending on the priority fee
  by sorting this list in a descending order of the priority fees*)
let rec refresh period =
  if true then (
    Snapshot.print_stats () ;
    Lwt_unix.sleep period >>= fun _ -> refresh period
  ) else
    Lwt.return (Ok ())

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
