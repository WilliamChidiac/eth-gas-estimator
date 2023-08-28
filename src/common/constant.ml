open Common_types

let verbose = ref Common_types.Default

let url =
  ref "wss://eth-mainnet.g.alchemy.com/v2/ytvWudRFU7i34JtwGZqu9MAynm_sUhK1"

let estimate = Q.of_float 15000000.

let max_coeff = Q.of_float 0.125

let refresh_rate = ref 10.

let min_pf = Z.of_int 0

let max_gu = 500_000

let gwei = true

let verbosity_filter = ref Common_types.Quiet

let lifespan = { delta_pending_tx = 4; delta_account = 3; delta_snapshot = 5 }

let snapshot_delay = ref 4.

let block_bs =
  ref
    [
      "0x1f9090aae28b8a3dceadf281b0f12828e676c326";
      "0x388c818ca8b9251b393131c08a736a67ccb19297";
      "0x95222290dd7278aa3ddd389cc1e1d165cc4bafe5";
      "0x690b9a9e9aa1c9db991c7721a92d351db4fac990";
      "0xdafea492d9c6733ae3d56b7ed1adb60692c98bc5";
    ]
