open Core.Std
open Async.Std

open Snaps_pervasives

let fetcher ~writer:w ~riak ~bucket () =
  let keys = Riak.fetch_keys_2i riak ~bucket in
  let fetch key =
    let kv = Riak.fetch_value riak ~bucket key in
    Pipe.write w kv
  in
  Deferred.List.iter keys ~f:fetch ~how:`Parallel >>= fun () ->
  Pipe.close w;
  return ()

let storer ~reader:r ~db ~bucket () =
  let rec store () =
      Pipe.read r >>= function
      | `Eof   ->
        Pipe.close_read r;
        return ()

      | `Ok kv ->
        Snaps_db.put db ~bucket kv;
        store ()
  in
  store ()

let start ~workers =
  Deferred.List.iter workers ~f:(fun w -> w ()) ~how:`Parallel

let main ~repo_path ~hostname ~port ~bucket =
  let db = Snaps_db.create ~path:repo_path in
  let riak = Riak.make ~hostname ~port () in
  let r, w = Pipe.create () in
  let workers =
    [ fetcher ~writer:w ~riak ~bucket
    ; storer  ~reader:r ~db   ~bucket
    ]
  in
  start ~workers >>= fun () ->
  shutdown 0;
  return ()

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
    )
    ( fun repo_path hostname port bucket () ->
        main ~repo_path ~hostname ~port ~bucket
    )
  |> Command.run
