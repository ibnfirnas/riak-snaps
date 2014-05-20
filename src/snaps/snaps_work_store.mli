open Core.Std
open Async.Std

val run
  :  object_queue:Snaps_object_info.t Pipe.Reader.t
  -> db:Snaps_db.t
  -> updates_channel:Snaps_work_progress.update_msg Pipe.Writer.t
  -> granularity:[`Bucket | `Object]
  -> unit Deferred.t
