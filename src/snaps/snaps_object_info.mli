open Core.Std
open Async.Std

type t

val of_riak_obj : Riak.Object.t -> t

val to_data_path : t -> string
(** Path to file containing data for the object. *)

val to_bucket_path : t -> string
(** Path to directory containing the object data file. *)
