open Core.Std
open Async.Std
open Composition

module Log = Snaps_log

let start ~workers =
  Deferred.List.iter workers ~f:(fun w -> w ()) ~how:`Parallel

let main ~repo_path ~hostname ~port ~bucket ~commits_before_gc =
  Log.init () >>= fun () ->
  Snaps_db.create ~path:repo_path ~commits_before_gc >>= fun db ->
  let riak_conn = Riak.Conn.make ~hostname ~port () in
  let r, w = Pipe.create () in
  let workers =
    [ Snaps_worker_fetch.create ~dst:w ~riak_conn ~riak_bucket:bucket
    ; Snaps_worker_store.create ~src:r ~db
    ]
  in
  start ~workers >>| fun () ->
  shutdown 0

let () =
  Command.async_basic
    ~summary:"Snapshot Riak objects to a Git repository."
    Command.Spec.(
      empty

      +> flag "-repo-path" (required string)
        ~doc:" Path to directory in which to store data"

      +> flag "-host" (optional_with_default "localhost" string)
        ~doc:" Riak hostname or IP address (default: localhost)"

      +> flag "-port" (optional_with_default 8098 int)
        ~doc:" Riak HTTP port (default: 8098)"

      +> flag "-bucket" (required string)
        ~doc:" Riak bucket to take snapshots from"

      +> flag "-commits-before-gc" (optional_with_default 100 int)
        ~doc:" How many commits to perform before pausing for GC? (default: 10)"
    )
    ( fun repo_path hostname port bucket commits_before_gc () ->
        main ~repo_path ~hostname ~port ~bucket ~commits_before_gc
    )
  |> Command.run
