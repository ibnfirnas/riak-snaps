open Core.Std
open Async.Std

type t

val of_riak_obj : Riak.Object.t -> t

val path_to_data : t -> string
(** Path to file containing data for the object. *)
