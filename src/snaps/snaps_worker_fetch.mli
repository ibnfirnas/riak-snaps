open Core.Std
open Async.Std

val create
  :  dst: Riak.Object.t Pipe.Writer.t
  -> riak_conn:Riak.Conn.t
  -> riak_bucket:string
  -> unit
  -> unit Deferred.t
