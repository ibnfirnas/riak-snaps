open Core.Std
open Async.Std

val run
  :  w: Snaps_object_info.t Pipe.Writer.t
  -> riak_conn:Riak.Conn.t
  -> riak_obj_ids:Riak.Object.ID.t list
  -> batch_size:int
  -> unit
  -> unit Deferred.t
