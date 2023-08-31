let pp_bz fmt x = Fmt.pf fmt "%d" (Z.to_int x)

let equal_bz a b = compare a b = 0

let bz = Alcotest.testable pp_bz equal_bz
