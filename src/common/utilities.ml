open Eth
open Common_types

(**conversion functions*)
let bzOption_to_float ?(def = Z.of_int 0) b =
  Z.to_float (Option.value ~default:def b) /. 1000000000.

let bz_to_float b = Z.to_float b /. 1000000000.

let print_trans (t : transaction) =
  Format.eprintf
    "transaction nonce: %d\n\
     gas: %d\n\
     priority fee per gas: %f\n\
     gas Price: %f\n\
     current base fee: %f\n\
     max Fee Per Gas: %f\n\
     max Priority Fee Per Gas: %f\n\
     Transaction Index: %d\n\n\n\
     @."
    t.tx_nonce t.gas
    ((Z.to_float t.gas_price /. 1000000000.) -. !Constant.current_base_fee)
    (Z.to_float t.gas_price /. 1000000000.)
    !Constant.current_base_fee
    (Z.to_float (Option.value ~default:(Z.of_int 0) t.max_fee_per_gas)
    /. 1000000000.)
    (Z.to_float (Option.value ~default:(Z.of_int 0) t.max_priority_fee_per_gas)
    /. 1000000000.)
    (Option.value ~default:(-1) t.transaction_index)

(** takes a response type and returns it in the form of a json string*)
let reponse_as_json (resp : response) : string =
  EzEncoding.construct response_enc resp

(**takes a json string and returns it in the form of a response type*)
let response_from_json (json : string) : response =
  EzEncoding.destruct response_enc json

(** calculate the base fee of the upcoming block,
    assuming that the "bf" parameter contains the necessary information of the last block added on the chain*)
let newBaseFee (bf : baseFee) =
  let base = float_of_string bf.base_fee in
  let gas = float_of_string bf.gas_used in
  (base
  +. base
     *. ((gas -. Constant.estimate) /. Constant.estimate *. Constant.max_coeff)
  )
  /. 1000000000.

let set_verbose v =
  begin
    Constant.verbose :=
      match v with
      | 0 -> Common_types.Quiet
      | 1 -> Common_types.Default
      | 2 -> Common_types.Verbose
      | 3 -> Common_types.Debug
      | 4 -> Common_types.Trace
      | _ -> Common_types.Trace
  end ;
  !Constant.verbose

let log ?(verbosity = !Constant.verbose) fmt =
  if verbosity <= !Constant.verbose then
    Format.eprintf fmt
  else
    Format.ifprintf Format.err_formatter fmt

let warning ?(verbosity = !Constant.verbose) fmt =
  if verbosity <= !Constant.verbose then begin
    Format.eprintf "Warning - " ;
    Format.eprintf fmt
  end else
    Format.ifprintf Format.err_formatter fmt
