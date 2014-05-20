open Core.Std
open Async.Std

type error = Unable_to_create_file of string
           | Unexpected_stderr     of string

val init
  :  unit
  -> unit Deferred.t

val add
  :  filepath:string
  -> (unit, error) Result.t Deferred.t

val add_exn
  :  filepath:string
  -> unit Deferred.t

val status
  :  filepath:string
  -> Git_status.t list Deferred.t

val commit
  :  msg:string
  -> (unit, error) Result.t Deferred.t

val commit_exn
  :  msg:string
  -> unit Deferred.t

val gc
  :  ?aggressive:bool
  -> unit
  -> unit Deferred.t
