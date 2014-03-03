open Core.Std
open Async.Std

val create
  :  src:Snaps_object_info.t Pipe.Reader.t
  -> db:Snaps_db.t
  -> unit
  -> unit Deferred.t
