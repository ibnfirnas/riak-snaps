open Core.Std
open Async.Std

val create
  :  src:Riak.Object.t Pipe.Reader.t
  -> db:Snaps_db.t
  -> unit
  -> unit Deferred.t
