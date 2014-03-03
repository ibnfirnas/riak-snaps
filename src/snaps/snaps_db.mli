open Core.Std
open Async.Std

type t

val create : path:string -> commits_before_gc:int -> t Deferred.t

val put
   : t
  -> Snaps_object_info.t
  -> unit Deferred.t
