open Alcotest
open Test_values

let test =
  Snapshot.print_local_stats (Some stats) ;
  [
    ( "tests stats",
      [
        test_case "number of transaction in block" `Quick (fun () ->
            check int "all accepted transactions should be seen"
              stats.sts_block_length 124);
        test_case "intersection" `Quick (fun () ->
            check bool "inter < total"
              (stats.sts_intersection < stats.sts_block_length)
              true);
        test_case "compare block length" `Quick (fun () ->
            let total = List.length stats.sts_compare in
            check int "all transaction in block have been stored" total 124);
        test_case "compare intersection" `Quick (fun () ->
            let inter =
              List.fold_left
                (fun i (_, is_in) ->
                  if is_in then
                    i + 1
                  else
                    i)
                0 stats.sts_compare in
            check int "inter seen = inter stored" inter stats.sts_intersection);
        test_case "base_fee value" `Quick (fun () ->
            check Testables.bz "base fee seen = base stored" stats.sts_base_fee
              (Z.of_int 26_124_073_870));
        test_case "gas used value" `Quick (fun () ->
            check int "gas used seen = gas used stored" stats.sts_gas_used
              10849992);
      ] );
    ( "test base fee formula",
      [
        test_case "gas used > 15 000 000" `Quick (fun () ->
            let x = make_bh "29094080218" "19638655" in
            check Testables.bz "" (Utilities.newBaseFee x)
              (Z.of_int 30_218_725_223));
        test_case "gas used < 15 000 000" `Quick (fun () ->
            let x = make_bh "29353783213" "11158650" in
            check Testables.bz "" (Utilities.newBaseFee x)
              (Z.of_int 28414131920));
        test_case "gas used = 15 000 000" `Quick (fun () ->
            let x = make_bh "29094080218" "15000000" in
            check Testables.bz "" (Utilities.newBaseFee x)
              (Z.of_int 29094080218));
      ] );
  ]

let () = run "local tests" test
