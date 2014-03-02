open Core.Std
open Async.Std

val create
  :  src:(string * string) Pipe.Reader.t
  -> db:Snaps_db.t
  -> bucket:string
  -> unit
  -> unit Deferred.t
