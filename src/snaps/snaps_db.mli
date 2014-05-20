open Core.Std
open Async.Std

type t

val create
  :  ?commits_before_gc_minor:int
  -> ?commits_before_gc_major:int
  -> path:string
  -> updates_channel:Snaps_work_progress.update_msg Pipe.Writer.t
  -> unit
  -> t Deferred.t

val put_object
   : t
  -> Snaps_object_info.t
  -> unit Deferred.t

val put_directory
   : t
  -> string
  -> unit Deferred.t

val gc_minor : t -> unit Deferred.t

val gc_major : t -> unit Deferred.t
