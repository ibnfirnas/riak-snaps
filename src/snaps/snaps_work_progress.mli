open Core.Std
open Async.Std

type update_msg  = [ `Fetched | `Committed | `Skipped ]

val run
  :  total_objects: int
  -> updates_channel: update_msg Pipe.Reader.t
  -> unit
  -> unit Deferred.t
