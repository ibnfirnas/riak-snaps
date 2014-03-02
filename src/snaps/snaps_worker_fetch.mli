open Core.Std
open Async.Std

val create
  :  dst:(string * string) Pipe.Writer.t
  -> riak:Riak.t
  -> bucket:string
  -> unit
  -> unit Deferred.t
