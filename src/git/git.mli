open Core.Std
open Async.Std

type status = Unchanged
            | Added
            | Modified
            | Unexpected of string

type error = Unable_to_create_file of string
           | Unexpected_stderr     of string

val init   :            unit -> unit Deferred.t

val add    : filepath:string -> (unit, error) Result.t Deferred.t

val status : filepath:string -> status Deferred.t

val commit :      msg:string -> (unit, error) Result.t Deferred.t

val gc     : ?aggressive:bool -> unit -> unit Deferred.t
