open Core.Std
open Async.Std
open Composition

module Log = Snaps_log.Make (struct let name = "Snaps_main" end)

let start ~workers =
  Deferred.List.iter workers ~f:(fun w -> w ()) ~how:`Parallel

let main
    ~repo_path
    ~hostname
    ~port
    ~bucket:riak_bucket
    ~commits_before_gc_minor
    ~commits_before_gc_major
    ~batch_size
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
    [ Snaps_worker_fetch.run ~w ~riak_conn ~riak_bucket ~batch_size
    ; Snaps_worker_store.run ~r ~db
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
      let batch_size              = 25
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

      +> flag "-batch-size" (optional_with_default Default.batch_size int)
        ~doc:(sprintf " How many objects to fetch at a time? (default: %d)" Default.batch_size)
    )
    ( fun repo_path
          hostname
          port
          bucket
          commits_before_gc_minor
          commits_before_gc_major
          batch_size
          ()
      ->
        main
          ~repo_path
          ~hostname
          ~port
          ~bucket
          ~commits_before_gc_minor
          ~commits_before_gc_major
          ~batch_size
    )
  |> Command.run
