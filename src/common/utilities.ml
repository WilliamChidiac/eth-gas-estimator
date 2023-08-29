open Eth
open Common_types

exception Tx_type_undefined of string

(**conversion functions*)

(**[print_in ~gwei value] helps in the printing of price values, 
    where [value] is Z.t mainly representing a fee in wei and 
    [gwei] is a boolean that determines wheter we should print [value] in wei or gwei*)
let print_in ?(gwei = Constant.gwei) wei =
  match gwei with
  | false -> Z.to_string wei
  | true ->
    Printf.sprintf "%.5f"
      (Q.to_float Q.(Q.of_bigint wei / Q.of_int 1_000_000_000))

(**[print_trans t base_fee] print relevant information on the transaction [t]. 
    the base_fee parameters helps calculates the priority_fee*)
let print_trans (t : transaction) base_fee =
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
    ((Z.to_float t.gas_price /. 1000000000.) -. base_fee)
    (Z.to_float t.gas_price /. 1000000000.)
    base_fee
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
let newBaseFee (bf : block_header) =
  let base = Q.of_string bf.base_fee in
  let gas = Q.of_string bf.gas_used in
  Q.to_bigint
    Q.(
      base
      + base
        * ((gas - Constant.estimate) / Constant.estimate * Constant.max_coeff))

(**[calc_priority_fee tx] returns the priority fee in a given time "through the base fee" of a transaction [tx] since it chages overtime depending on the base fee.
  the priority fee's formula depends on the type of the transaction [tx] :
   for [tx] of type 0 and 1
    priority fee = gas price - base fee
  for [tx] of type 2 
    priority fee = min (max fee per gas - base fee , max priority fee)*)
let calc_priority_fee bf tx =
  match tx.typ with
  | None -> Z.of_int 0
  | Some 0 | Some 1 -> Z.(tx.gas_price - bf)
  | Some 2 ->
    let a_mpfpg =
      Option.value ~default:(Z.of_int 0) tx.max_priority_fee_per_gas in
    let a_mpfg = Option.value ~default:(Z.of_int 0) tx.max_fee_per_gas in
    min a_mpfpg Z.(a_mpfg - bf)
  | Some x ->
    raise
      (Tx_type_undefined (Printf.sprintf "Unknown type of transaction : %d" x))

let compare_by_priority_fee bf a b =
  let x = compare (calc_priority_fee bf b) (calc_priority_fee bf a) in
  if x = 0 then
    compare b.tx_hash a.tx_hash
  else
    x

(**[min_gas_price bf] given a base fee, this function returns the minimum gas price that a sender should input to consider adding it's transaction in out mempool. 
    this function is mainly used to blakclist the accounts and filter the mempool.*)
let min_priority_fee bf =
  let rate = Q.of_float 0.94166666 in
  let power x n =
    let rec aux n i =
      if n = 0 then
        i
      else
        aux (n - 1) Q.(i * x) in
    aux n (Q.of_int 1) in
  Z.(Q.to_bigint Q.(power rate 4 * Q.of_bigint bf) - bf)

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
  if verbosity >= !Constant.verbose then
    Format.eprintf fmt
  else
    Format.ifprintf Format.err_formatter fmt

let warning ?(verbosity = !Constant.verbose) fmt =
  if verbosity >= !Constant.verbose then begin
    Format.eprintf "Warning - " ;
    Format.eprintf fmt
  end else
    Format.ifprintf Format.err_formatter fmt

let to_string_verbosity = function
  | Common_types.Quiet -> "Quiet"
  | Common_types.Default -> "Default"
  | Common_types.Verbose -> "verbose"
  | Common_types.Debug -> "Debug"
  | Common_types.Trace -> "Trace"
