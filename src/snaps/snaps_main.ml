open Core.Std
open Async.Std

open Snaps_pervasives

let fetcher ~writer:w ~riak ~bucket () =
  Log.Global.info "Worker \"fetcher\" BEGIN";
  Log.Global.flushed () >>= fun () ->
  Log.Global.info "Fetch  : keys of %s. Via 2i" bucket;
  Log.Global.flushed () >>= fun () ->
  Riak.fetch_keys_2i riak ~bucket >>= fun keys ->
  let fetch key =
    Log.Global.info "Fetch  : %S" (bucket ^ "/" ^ key);
    Log.Global.flushed () >>= fun () ->
    Riak.fetch_value riak ~bucket key >>= fun kv ->
    Pipe.write w kv
  in
  Deferred.List.iter keys ~f:fetch ~how:`Parallel >>= fun () ->
  Pipe.close w;
  Log.Global.info "Worker \"fetcher\" END";
  Log.Global.flushed ()

let storer ~reader:r ~db ~bucket () =
  Log.Global.info "Worker \"storer\" BEGIN";
  Log.Global.flushed () >>= fun () ->
  let rec store () =
    Pipe.read r >>= function
    | `Eof   ->
      Pipe.close_read r;
      return ()

    | `Ok kv ->
      Snaps_db.put db ~bucket kv >>= fun () ->
      store ()
  in
  store () >>= fun () ->
  Log.Global.info "Worker \"storer\" END";
  Log.Global.flushed ()

let start ~workers =
  Deferred.List.iter workers ~f:(fun w -> w ()) ~how:`Parallel

let main ~repo_path ~hostname ~port ~bucket ~commits_before_gc =
  Log.Global.set_level `Debug;
  Log.Global.set_output [Log.Output.stderr ()];

  Snaps_db.create ~path:repo_path ~commits_before_gc >>= fun db ->
  let riak = Riak.make ~hostname ~port () in
  let r, w = Pipe.create () in
  let workers =
    [ fetcher ~writer:w ~riak ~bucket
    ; storer  ~reader:r ~db   ~bucket
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
