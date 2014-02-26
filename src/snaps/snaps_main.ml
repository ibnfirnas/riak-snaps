open Printf
open Snaps_pervasives

let () =
  let repo_path, hostname, bucket =
    try
      Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)
    with Invalid_argument "index out of bounds" ->
      eprintf "USAGE: %s repo_path hostname bucket\n%!" Sys.argv.(0);
      exit 1
  in
  let db = Snaps_db.create ~path:repo_path in
  let riak = Riak.make ~hostname () in
  List.iter
    (Riak.fetch_value riak ~bucket |- Snaps_db.put db ~bucket)
    (Riak.fetch_keys  riak ~bucket)
