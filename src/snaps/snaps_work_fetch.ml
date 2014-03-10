open Core.Std
open Async.Std

module Ash = Async_shell
module Log = Snaps_log.Make (struct let name = "Snaps_work_fetch" end)

type t = { riak_conn       : Riak.Conn.t
         ; object_queue    : (Snaps_object_info.t Pipe.Writer.t) option
         ; updates_channel : Snaps_work_progress.update_msg Pipe.Writer.t
         }

let fetch_object t id =
  let object_name = Riak.Object.ID.to_string id in
  Log.info (sprintf "Fetch BEGIN: %S" object_name) >>= fun () ->
  Riak.Object.fetch t.riak_conn id
  >>= fun obj ->
  let info = Snaps_object_info.of_riak_obj obj in
  let path = Snaps_object_info.path_to_data info in
  let data = obj.Riak.Object.data in
  Log.info (sprintf "Write BEGIN: %S" path) >>= fun () ->
  Ash.mkdir ~p:() (Filename.dirname path)   >>= fun () ->
  Writer.save path ~contents:data           >>= fun () ->
  Log.info (sprintf "Write END: %S" path)   >>= fun () ->
  begin match t.object_queue with
  | None              -> ()
  | Some object_queue -> Pipe.write_without_pushback object_queue info
  end;
  return () >>= fun () ->
  Log.info (sprintf "Fetch END: %S" object_name) >>= fun () ->
  Pipe.write_without_pushback t.updates_channel `Fetched;
  return ()

let fetch_objects t ids ~batch_size =
  let rec fetch_batch () =
    Pipe.read_at_most ids ~num_values:batch_size
    >>= function
      | `Eof      -> return ()
      | `Ok batch ->
        Deferred.Queue.iter batch ~how:`Parallel ~f:(fetch_object t)
        >>= fun () ->
        fetch_batch ()
  in
  fetch_batch ()

let run ~object_queue ~riak_conn ~riak_obj_ids ~batch_size ~updates_channel =
  let t = {riak_conn; object_queue; updates_channel} in
  Log.info "Worker STARTED" >>= fun () ->
  fetch_objects t (Pipe.of_list riak_obj_ids) ~batch_size >>= fun () ->
  Log.info "Worker FINISHED"
