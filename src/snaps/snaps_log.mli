open Core.Std
open Async.Std

module type CALLER = sig
  val name : string
end

module Make (Caller : CALLER) : sig
  val init  : unit   -> unit Deferred.t
  val info  : string -> unit Deferred.t
  val error : string -> unit Deferred.t
end
