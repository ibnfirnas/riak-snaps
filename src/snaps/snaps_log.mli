open Core.Std
open Async.Std

val init : unit -> unit Deferred.t

val info : string -> unit Deferred.t
