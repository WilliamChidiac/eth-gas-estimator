open Common_types

let read_json filename enc =
  Core.In_channel.read_all filename |> EzEncoding.destruct enc

let stats = read_json "./test/stats.json" Snapshot.local_stats_enc

let mempool = read_json "./test/mempool.json" Api_server.mempool_enc

let all = read_json "./test/snapshot.json" Api_server.snapshot_enc

let make_bh bf gu =
  {
    base_fee = bf;
    gas_used = gu;
    timestamp = Int64.of_int 0;
    number = 0;
    fee_recepient = "";
  }
