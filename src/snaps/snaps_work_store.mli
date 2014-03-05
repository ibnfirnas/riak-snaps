open Core.Std
open Async.Std

val run
  :  r:Snaps_object_info.t Pipe.Reader.t
  -> db:Snaps_db.t
  -> unit
  -> unit Deferred.t
