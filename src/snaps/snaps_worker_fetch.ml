open Core.Std
open Async.Std

module Ash = Async_shell
module Log = Snaps_log.Make (struct let name = "Snaps_worker_fetch" end)

type t = { riak_conn : Riak.Conn.t
         ; w         : Snaps_object_info.t Pipe.Writer.t
         }

let fetch_object {riak_conn; w} id =
  let object_name = Riak.Object.ID.to_string id in
  Log.info (sprintf "Fetch BEGIN: %S" object_name)                >>= fun () ->
  Riak.Object.fetch riak_conn id                                  >>= fun obj ->
  let info = Snaps_object_info.of_riak_obj obj in
  let path = Snaps_object_info.path_to_data info in
  let data = obj.Riak.Object.data in
  Log.info (sprintf "Write BEGIN: %S" path)                        >>= fun () ->
  Ash.mkdir ~p:() (Filename.dirname path)                          >>= fun () ->
  Writer.save path ~contents:data                                  >>= fun () ->
  Log.info (sprintf "Write END: %S" path)                          >>= fun () ->
  Pipe.write_without_pushback w info;
  return ()                                                        >>= fun () ->
  Log.info (sprintf "Fetch END: %S" object_name)

let fetch_objects t ids ~batch_size =
  let rec fetch_batch () =
    Pipe.read_at_most ids ~num_values:batch_size >>= function
    | `Eof      -> return ()
    | `Ok batch ->
      Deferred.Queue.iter batch ~how:`Parallel ~f:(fetch_object t) >>= fun () ->
      fetch_batch ()
  in
  fetch_batch ()

let run ~w ~riak_conn ~riak_bucket ~batch_size () =
  let t = {riak_conn; w} in
  Log.info "Worker STARTED"                           >>= fun () ->
  Log.info (sprintf "Fetch BEGIN: keys of %s. Via 2i" riak_bucket) >>= fun () ->
  Riak.Object.ID.fetch_via_2i riak_conn ~bucket:riak_bucket        >>= fun ids ->
  let ids = Pipe.of_list ids in
  Log.info (sprintf "Fetch END: keys of %s. Via 2i" riak_bucket)   >>= fun () ->
  fetch_objects t ids ~batch_size                                  >>= fun () ->
  Pipe.close w;
  Log.info "Worker FINISHED"
