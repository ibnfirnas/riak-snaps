open Core.Std
open Async.Std

module Log = Snaps_log.Make (struct let name = "Snaps_main" end)

let run ~workers =
  Deferred.List.iter workers ~f:(fun w -> w ()) ~how:`Parallel

let main_full
    ~repo_path
    ~hostname
    ~port
    ~bucket:riak_bucket
    ~commits_before_gc_minor
    ~commits_before_gc_major
    ~batch_size
    ~commit_granularity
  =
  Snaps_log.init ~level:`Info ~repo_path;
  let riak_conn = Riak.Conn.make ~hostname ~port () in
  Log.info (sprintf "Fetch BEGIN: keys of %s. Via 2i" riak_bucket) >>= fun () ->
  Riak.Object.ID.fetch_via_2i riak_conn ~bucket:riak_bucket
  >>= fun riak_obj_ids ->
  Log.info (sprintf "Fetch END: keys of %s. Via 2i" riak_bucket) >>= fun () ->
  let object_queue_r , object_queue_w = Pipe.create () in
  let updates_r      , updates_w      = Pipe.create () in
  Snaps_db.create
    ~path:repo_path
    ~updates_channel:updates_w
    ~commits_before_gc_minor
    ~commits_before_gc_major
    ()
  >>= fun db ->
  let total_objects = List.length riak_obj_ids in

  let worker_progress () =
    Snaps_work_progress.run
      ~total_objects
      ~updates_channel:updates_r
  in
  let worker_fetcher () =
    Snaps_work_fetch.run
      ~object_queue:(Some object_queue_w)
      ~riak_conn
      ~riak_obj_ids
      ~batch_size
      ~updates_channel:updates_w
    >>| fun () ->
    Pipe.close object_queue_w
  in
  let worker_storer () =
    Snaps_work_store.run
      ~object_queue:object_queue_r
      ~db
      ~updates_channel:updates_w
      ~granularity:commit_granularity
    >>| fun () ->
    Pipe.close updates_w
  in
  let workers =
    [ worker_progress
    ; worker_fetcher
    ; worker_storer
    ]
  in
  run ~workers

let main_fetch
    ~repo_path
    ~hostname
    ~port
    ~bucket:riak_bucket
    ~batch_size
  =
  Snaps_log.init ~level:`Info ~repo_path;
  let riak_conn = Riak.Conn.make ~hostname ~port () in
  Log.info (sprintf "Fetch BEGIN: keys of %s. Via 2i" riak_bucket) >>= fun () ->
  Riak.Object.ID.fetch_via_2i riak_conn ~bucket:riak_bucket
  >>= fun riak_obj_ids ->
  Log.info (sprintf "Fetch END: keys of %s. Via 2i" riak_bucket) >>= fun () ->
  let updates_r      , updates_w      = Pipe.create () in
  Snaps_db.create ~path:repo_path ~updates_channel:updates_w () >>= fun db ->
  ignore db;
  let total_objects = List.length riak_obj_ids in
  let worker_progress () =
    Snaps_work_progress.run
      ~total_objects
      ~updates_channel:updates_r
  in
  let worker_fetcher () =
    Snaps_work_fetch.run
      ~object_queue:None
      ~riak_conn
      ~riak_obj_ids
      ~batch_size
      ~updates_channel:updates_w
    >>| fun () ->
    Pipe.close updates_w
  in
  let workers =
    [ worker_progress
    ; worker_fetcher
    ]
  in
  run ~workers

module UI : sig
  val spec : Command.t
end = struct
  module Flag = struct
    open Command.Spec

    module Default = struct
      let riak_host               = "localhost"
      let riak_port               = 8098
      let commits_before_gc_minor = 100
      let commits_before_gc_major = 500
      let batch_size              = 25
      let commit_granularity      = "bucket"
    end

    let repo_path =
      flag "-repo-path" (required string)
      ~doc:" Path to directory in which to store data"

    let host =
      flag "-host" (optional_with_default Default.riak_host string)
      ~doc:
      ( " Riak hostname or IP address"
      ^ sprintf "(default: %s)" Default.riak_host
      )

    let port =
      flag "-port" (optional_with_default Default.riak_port int)
      ~doc:(sprintf " Riak HTTP port (default: %d)" Default.riak_port)

    let bucket =
      flag "-bucket" (required string)
      ~doc:" Riak bucket to take snapshots from"

    let commits_before_gc_minor =
      flag
        "-commits-before-gc-minor"
        (optional_with_default Default.commits_before_gc_minor int)
        ~doc:
        ( " How many commits to perform before pausing for minor/normal GC?"
        ^ sprintf "(default: %d)" Default.commits_before_gc_minor
        )

    let commits_before_gc_major =
      flag
        "-commits-before-gc-major"
        (optional_with_default Default.commits_before_gc_major int)
        ~doc:
        ( " How many commits to perform before pausing for major/aggressive GC?"
        ^ sprintf "(default: %d)" Default.commits_before_gc_major
        )

    let batch_size =
      flag "-batch-size" (optional_with_default Default.batch_size int)
      ~doc:
      ( " How many objects to fetch at a time?"
      ^ sprintf "(default: %d)" Default.batch_size
      )

    let commit_granularity =
      flag
        "-commit-granularity"
        (optional_with_default Default.commit_granularity string)
        ~doc:
        ( " How often to commit? Per [bucket | object]? "
        ^ sprintf "(default: %s)" Default.commit_granularity
        )
  end

  let full =
    let (+) = Command.Spec.(+>) in
    Command.async_basic
      ~summary:"Full run: fetch and commit."
      ( Command.Spec.empty
      + Flag.repo_path
      + Flag.host
      + Flag.port
      + Flag.bucket
      + Flag.commits_before_gc_minor
      + Flag.commits_before_gc_major
      + Flag.batch_size
      + Flag.commit_granularity
      )
      ( fun repo_path
            hostname
            port
            bucket
            commits_before_gc_minor
            commits_before_gc_major
            batch_size
            commit_granularity
            ()
        ->
          let commit_granularity =
            match commit_granularity with
            | "bucket" -> `Bucket
            | "object" -> `Object
            | other    ->
                failwith (sprintf "Unknown commit granularity: %S\n" other)
          in
          main_full
            ~repo_path
            ~hostname
            ~port
            ~bucket
            ~commits_before_gc_minor
            ~commits_before_gc_major
            ~batch_size
            ~commit_granularity
      )

  let fetch =
    let (+) = Command.Spec.(+>) in
    Command.async_basic
      ~summary:"Fetch-only. Do not commit."
      ( Command.Spec.empty
      + Flag.repo_path
      + Flag.host
      + Flag.port
      + Flag.bucket
      + Flag.batch_size
      )
      ( fun repo_path
            hostname
            port
            bucket
            batch_size
            ()
        ->
          main_fetch
            ~repo_path
            ~hostname
            ~port
            ~bucket
            ~batch_size
      )

  let spec =
    Command.group
      ~summary:"Snapshot Riak objects to a Git repository."
      [ "full"  , full
      ; "fetch" , fetch
      ]
end

let () =
  Command.run UI.spec
