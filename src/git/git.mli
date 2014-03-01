open Core.Std
open Async.Std

type status = Unchanged
            | Added
            | Modified
            | Unexpected of string

val init   :            unit -> unit Deferred.t

val add    : filepath:string -> unit Deferred.t

val status : filepath:string -> status Deferred.t

val commit :      msg:string -> unit Deferred.t
