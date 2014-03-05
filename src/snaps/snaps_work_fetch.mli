open Core.Std
open Async.Std

val run
  :  object_queue: Snaps_object_info.t Pipe.Writer.t
  -> riak_conn:Riak.Conn.t
  -> riak_obj_ids:Riak.Object.ID.t list
  -> batch_size:int
  -> updates_channel: Snaps_work_progress.update_msg Pipe.Writer.t
  -> unit
  -> unit Deferred.t
