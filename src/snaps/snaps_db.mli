open Core.Std
open Async.Std

type t

val create
  :  path:string
  -> updates_channel:Snaps_work_progress.update_msg Pipe.Writer.t
  -> commits_before_gc_minor:int
  -> commits_before_gc_major:int
  -> t Deferred.t

val put
   : t
  -> Snaps_object_info.t
  -> unit Deferred.t

val gc_minor : t -> unit Deferred.t

val gc_major : t -> unit Deferred.t
