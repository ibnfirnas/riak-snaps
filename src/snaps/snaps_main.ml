open Core.Std
open Async.Std
open Composition

module Log = Snaps_log

let start ~workers =
  Deferred.List.iter workers ~f:(fun w -> w ()) ~how:`Parallel

let main
    ~repo_path
    ~hostname
    ~port
    ~bucket
    ~commits_before_gc_minor
    ~commits_before_gc_major
  =
  Log.init () >>= fun () ->
  Snaps_db.create
    ~path:repo_path
    ~commits_before_gc_minor
    ~commits_before_gc_major
  >>= fun db ->
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
  let
    module Default = struct
      let riak_host               = "localhost"
      let riak_port               = 8098
      let commits_before_gc_minor = 100
      let commits_before_gc_major = 500
    end
  in
  Command.async_basic
    ~summary:"Snapshot Riak objects to a Git repository."
    Command.Spec.(
      empty

      +> flag "-repo-path" (required string)
        ~doc:" Path to directory in which to store data"

      +> flag "-host" (optional_with_default Default.riak_host string)
        ~doc:(sprintf " Riak hostname or IP address (default: %s)" Default.riak_host)

      +> flag "-port" (optional_with_default Default.riak_port int)
        ~doc:(sprintf " Riak HTTP port (default: %d)" Default.riak_port)

      +> flag "-bucket" (required string)
        ~doc:" Riak bucket to take snapshots from"

      +> flag
        "-commits-before-gc-minor"
        (optional_with_default Default.commits_before_gc_minor int)
        ~doc:(sprintf " How many commits to perform before pausing for minor/normal GC? (default: %d)" Default.commits_before_gc_minor)

      +> flag
        "-commits-before-gc-major"
        (optional_with_default Default.commits_before_gc_major int)
        ~doc:(sprintf " How many commits to perform before pausing for major/aggressive GC? (default: %d)" Default.commits_before_gc_major)
    )
    ( fun repo_path
          hostname
          port
          bucket
          commits_before_gc_minor
          commits_before_gc_major
          ()
      ->
        main
          ~repo_path
          ~hostname
          ~port
          ~bucket
          ~commits_before_gc_minor
          ~commits_before_gc_major
    )
  |> Command.run
