open Eth

(* Command types *)
type verbose =
  | Quiet (* 0- No extra message *)
  | Default (* 1- Brief *)
  | Verbose (* 2- Descriptive description *)
  | Debug (* 3- Internal execution message *)
  | Trace (* 4- Print everything *)

type vanilla_options = {
  vanilla_option_1 : string option; [@key "vanilla-option-1"]
  vanilla_option_2 : string option; [@key "vanilla-option-2"]
}
[@@deriving arg]

type alchemy_options = {
  alchemy_option_1 : string option;
      [@dft Some "option-par-defaut"] [@key "alchemy-option-1"]
  alchemy_option_2 : string option; [@dft None] [@key "alchemy-option-2"]
}
[@@deriving arg]

type command =
  | Vanilla of vanilla_options [@key "vanilla-mode"]
  | Alchemy of alchemy_options [@key "alchemy-mode"]
[@@deriving arg]

type config = {
  opt_node_url : string option; [@dft None] [@key "node-url"]
  verbose : int; [@dft 1] [@global]
  command : command;
  version : unit;
      [@conv
        fun _ ->
          Version.pp_version () ;
          exit 0]
}
[@@deriving arg { exe = "main.exe" }]

(* Ethereum types *)

(**request messages*)
type requests_params = {
  rq_rpc_version : string; [@key "jsonrpc"]
  rq_id : int; [@key "id"]
  rq_method : string; [@key "method"]
  rq_params : string list; [@key "params"]
}
[@@deriving encoding { ignore }]

(*response messages*)

(*Connection complete messages*)
type conn = {
  conn_rpc_version : string; [@key "jsonrpc"]
  conn_id : int; [@key "id"]
  conn_result : string; [@key "result"]
}
[@@deriving encoding]

(**contains all the important information to calculate the base fee of the upcomping block*)
type baseFee = {
  base_fee : string; [@key "baseFeePerGas"]
  gas_used : string; [@key "gasUsed"]
  timestamp : bint64; [@key "timestamp"]
  number : bint; [@key "number"]
}
[@@deriving encoding { ignore }]

(*mined transaction type*)
type mined_tx = {
  tx_tx : transaction; [@key "transaction"]
  tx_removed : bool; [@key "removed"]
}
[@@deriving encoding]

(*transactions pending or accepted OR block headers *)
type res_ws =
  | Mined_transaction of mined_tx
  | Pending_transaction of transaction
  | Base_fee of baseFee
[@@deriving encoding { ignore }]

type 'a result = ('a[@wrap "result"]) [@@deriving encoding { ignore }]

type 'a params = ('a[@wrap "params"]) [@@deriving encoding { ignore }]

type data = res_ws result params [@@deriving encoding { ignore }]

type response =
  | Data of data
  | Connection of conn
[@@deriving encoding]
