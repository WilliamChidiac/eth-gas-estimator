open Lwt.Infix
open Eth
open Common_types
open Sorted_list
open Utilities

(*websocket url*)
let uri = "wss://eth-mainnet.g.alchemy.com/v2/ytvWudRFU7i34JtwGZqu9MAynm_sUhK1"

(* alchemy_minedTransactions :  return all the transaction in a newBlock
   alchemy_pendingTransactions : return all the pending transaction in the mempool
   newHeads : return all the newly added blocks
*)

(*request messages*)

let request_pending =
  Common_types.alchemy_subscription "alchemy_pendingTransactions"

let request_mined =
  Common_types.alchemy_subscription "alchemy_minedTransactions"

let request_new_block = Common_types.alchemy_subscription "newHeads"

let accepted_tx = ref empty
(* let delta = 6 *)
(* let cleaner = Hashtbl.create delta *)

(**this function will be called every time we receive a notification. the receiveed message will then be treated depending on it's type.*)
let react (_w : string EzWs.action) s =
  Lwt.catch
    (fun () ->
      Lwt.return (response_from_json s) >>= fun resp ->
      match resp with
      | Connection _ ->
        Format.eprintf "Connection complete.@." ;
        Lwt.return (Ok ()) (*connected messages*)
      | Data d -> (
        match d with
        | Mined_transaction t ->
          remove_tx t.tx_tx ;
          add_tx t.tx_tx accepted_tx ;
          Lwt.return (Ok ())
          (*if we received a mined transaction, we'll remove it from the list of pending transactioin*)
        | Pending_transaction t ->
          add_tx t l_pfpg ;
          Lwt.return (Ok ())
          (*if we received a pending trasnaction, we'll add it to the list o pending*)
        | Base_fee b ->
          l_pfpg := sort_by compare_by_priority_fee ;
          current_base_fee := newBaseFee b ;
          print_x_first !l_pfpg 10 ;
          Format.eprintf "\n\nBASE FEE : %f@." !current_base_fee ;
          Lwt.return (Ok ()))
      (*if we received a block header, we'll be updating the base fee of the new upcoming block*))
    (fun exn ->
      Format.eprintf "exn:%s\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n@."
        (Printexc.to_string exn) ;
      Lwt.return (Ok ()))

let error _ _ = failwith "error"

(**this function will be called every "period" seconds where period is a float.
  it uses the list of pending to calculate the probabilities of inclusion in the new block depending on the priority fee
  by sorting this list in a descending order of the priority fees*)
let rec calculate period =
  let rec aux l gu maxP =
    match l with
    | [] ->
      Format.eprintf "nombre total de gas: %f\n@." gu ;
      maxP /. gu
    | e :: l_aux ->
      if gu > 7000000. then
        let x = calc_priority_fee e in
        if x < 0. then (
          Format.eprintf
            "transaction type : %d\ntrasaction priority fee: %f\n@."
            (Option.value ~default:(-1) e.typ)
            x ;
          (* print_trans e; *)
          x
        ) else (
          Format.eprintf "x = %f@." x ;
          x
        )
      else
        let gas = float_of_int e.gas in
        aux l_aux (gu +. gas)
          (maxP +. (bzOption_to_float e.max_priority_fee_per_gas *. gas)) in
  if true then (
    Lwt_unix.sleep period >>= fun _ ->
    Mutex.lock mutex ;

    (* print_container !accepted_tx; *)
    l_pfpg := sort_by compare_by_priority_fee ;

    (* Format.eprintf "propability\nPriority Fee Per Gas: %f\n@." (aux !l_pfpg 0. 0.);  *)
    let _ = aux !l_pfpg 0. 0. in
    Mutex.unlock mutex ;
    Lwt.return_unit
    >>=
    (* fun _ ->  Format.eprintf "nombre de transaction total : %d\nnombre de transaction valider : %d\nnombre de transaction enleve:%d@."
                                       (get_size ())                   !total_accepted_tx                    !total_removed_tx;
       total_accepted_tx := 0;
       total_removed_tx := 0; *)
    fun _ -> calculate period
  ) else
    Lwt.return (Ok ())

let request () =
  Lwt_main.run
    (Printf.printf "Connecting to %s\n%!" uri ;
     EzWs.connect ~error ~react uri >>= function
     | Error e ->
       prerr_endline e ;
       Lwt.return (Ok ())
     | Ok ws ->
       ws.action.send request_pending >>= fun _ ->
       ws.action.send request_mined >>= fun _ ->
       ws.action.send request_new_block >>= fun _ ->
       calculate 0.5 >>= fun _ -> Lwt.return (Ok ()))
