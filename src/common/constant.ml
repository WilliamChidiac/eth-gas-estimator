open Common_types

let verbose = ref Common_types.Default

let estimate = Q.of_float 15000000.

let max_coeff = Q.of_float 0.125

let refresh_rate = ref 10.

let min_pf = Z.of_int 0

let max_gu = 500_000

let gwei = true

let verbosity_filter = ref Common_types.Quiet

let lifespan = { delta_pending_tx = 4; delta_account = 3; delta_snapshot = 5 }
