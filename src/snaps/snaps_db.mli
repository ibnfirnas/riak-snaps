open Core.Std
open Async.Std

type t

val create : path:string -> t Deferred.t

val put
   : t
  -> bucket:string
  -> string * string  (* Key/Value pair. Unlabled for partial application. *)
  -> unit Deferred.t
