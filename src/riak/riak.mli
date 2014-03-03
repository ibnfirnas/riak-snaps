open Core.Std
open Async.Std

module Conn : sig
  type t

  val make : ?hostname:string -> ?port:int -> unit -> t
end

module Object : sig
  module ID : sig
    type t = { bucket : string
             ; key    : string
             }

    val to_string : t -> string

    val fetch_via_2i
      :  Conn.t
      -> bucket:string
      -> (t list) Deferred.t

    val fetch_via_brute_force
      :  Conn.t
      -> bucket:string
      -> (t list) Deferred.t
  end

  type t = { id   : ID.t
           ; data : string
           }

  val fetch
     : Conn.t
    -> ID.t
    -> t Deferred.t
end
