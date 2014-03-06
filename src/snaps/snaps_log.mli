open Core.Std
open Async.Std

module type CALLER = sig
  val name : string
end

module Make (Caller : CALLER) : sig
  val info  : string -> unit Deferred.t
  val error : string -> unit Deferred.t
end

val init : level:Log.Level.t -> repo_path:string -> unit
