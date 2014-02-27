open Core.Std
open Async.Std

open Snaps_pervasives

let main ~repo_path ~hostname ~port ~bucket =
  let db = Snaps_db.create ~path:repo_path in
  let riak = Riak.make ~hostname ~port () in
  let keys = Riak.fetch_keys_2i riak ~bucket in
  let fetch = Riak.fetch_value riak ~bucket in
  let store = Snaps_db.put db ~bucket in
  Deferred.List.iter keys ~f:(fetch |- store |- return) ~how:`Parallel

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
    (fun repo_path hostname port bucket () -> main ~repo_path ~hostname ~port ~bucket)
  |> Command.run
