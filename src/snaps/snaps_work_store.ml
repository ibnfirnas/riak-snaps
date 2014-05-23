open Core.Std
open Async.Std

module Log = Snaps_log.Make (struct let name = "Snaps_work_store" end)

type t = { db              : Snaps_db.t
         ; object_queue    : Snaps_object_info.t            Pipe.Reader.t
         ; updates_channel : Snaps_work_progress.update_msg Pipe.Writer.t
         }

let rec store_objects t =
  let {object_queue; db; updates_channel} = t in
  Pipe.read object_queue
  >>= function
    | `Eof            -> return ()
    | `Ok object_info -> begin
      Snaps_db.put_object db object_info >>= fun () ->
      store_objects t
    end

let store_bucket t =
  let {object_queue; db; updates_channel} = t in
  Pipe.read_all object_queue >>= fun object_queue ->
  let paths = String.Hash_set.create () in
  Queue.iter object_queue ~f:(
    fun obj ->
      Hash_set.add paths (Snaps_object_info.to_bucket_path obj)
  );
  let f = Snaps_db.put_directory db in
  Deferred.List.iter (Hash_set.to_list paths) ~how:`Sequential ~f

let run ~object_queue ~db ~updates_channel ~granularity =
  Log.info "Worker STARTED" >>= fun () ->
  let store =
    match granularity with
    | `Object -> store_objects
    | `Bucket -> store_bucket
  in
  let t = {object_queue; db; updates_channel} in
  store t              >>= fun () ->
  Snaps_db.gc_major db >>= fun () ->
  Log.info "Worker FINISHED"
