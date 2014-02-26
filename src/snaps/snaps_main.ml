open StdLabels
open Printf
open Snaps_pervasives

type options =
  { repo_path : string
  ; hostname  : string
  ; bucket    : string
  }

let take_snapshots ~repo_path ~hostname ~bucket =
  let db = Snaps_db.create ~path:repo_path in
  let riak = Riak.make ~hostname () in
  let keys = Riak.fetch_keys riak ~bucket in
  let fetch = Riak.fetch_value riak ~bucket in
  let store = Snaps_db.put db ~bucket in
  List.iter keys ~f:(fetch |- store)

let get_opts () =
    try
      { repo_path = Sys.argv.(1)
      ; hostname  = Sys.argv.(2)
      ; bucket    = Sys.argv.(3)
      }
    with Invalid_argument "index out of bounds" ->
      eprintf "USAGE: %s repo_path hostname bucket\n%!" Sys.argv.(0);
      exit 1

let main () =
  let {repo_path; hostname; bucket} = get_opts () in
  take_snapshots ~repo_path ~hostname ~bucket

let () = main ()
