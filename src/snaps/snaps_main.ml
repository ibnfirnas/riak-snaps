open Core.Std
open Async.Std

open Snaps_pervasives

type options =
  { repo_path : string
  ; hostname  : string
  ; bucket    : string
  }

let take_snapshots ~repo_path ~hostname ~bucket =
  let db = Snaps_db.create ~path:repo_path in
  let riak = Riak.make ~hostname () in
  let keys = Riak.fetch_keys_2i riak ~bucket in
  let fetch = Riak.fetch_value riak ~bucket in
  let store = Snaps_db.put db ~bucket in
  List.iter keys ~f:(fetch |- store)

let get_opts () =
    try Ok { repo_path = Sys.argv.(1)
           ; hostname  = Sys.argv.(2)
           ; bucket    = Sys.argv.(3)
           }
    with Invalid_argument "index out of bounds" ->
      Error `Invalid_argument

let main () =
  begin match get_opts () with
  | Ok {repo_path; hostname; bucket} ->
    take_snapshots ~repo_path ~hostname ~bucket;
    shutdown 0;

  | Error `Invalid_argument ->
    eprintf "USAGE: %s repo_path hostname bucket\n%!" Sys.argv.(0);
    shutdown 1
  end;
  never_returns (Scheduler.go ())

let () = main ()
