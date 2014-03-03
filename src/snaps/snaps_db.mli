open Core.Std
open Async.Std

type t

val create
  :  path:string
  -> commits_before_gc_minor:int
  -> commits_before_gc_major:int
  -> t Deferred.t

val put
   : t
  -> Snaps_object_info.t
  -> unit Deferred.t
