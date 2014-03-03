open Core.Std
open Async.Std

type t

val create : path:string -> commits_before_gc:int -> t Deferred.t

val put
   : t
  -> Riak.Object.t
  -> unit Deferred.t
