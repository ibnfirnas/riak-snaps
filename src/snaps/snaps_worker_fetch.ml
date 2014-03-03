open Core.Std
open Async.Std

module Ash = Async_shell
module Log = Snaps_log

type t = { riak_conn   : Riak.Conn.t
         ; dst         : Snaps_object_info.t Pipe.Writer.t
         }

let fetch_object {riak_conn; dst} id =
  Log.info (sprintf "Fetch  : %S" (Riak.Object.ID.to_string id)) >>= fun () ->
  Riak.Object.fetch riak_conn id                                 >>= fun obj ->
  let info = Snaps_object_info.of_riak_obj obj in
  let path = Snaps_object_info.path_to_data info in
  let data = obj.Riak.Object.data in
  Ash.mkdir ~p:() (Filename.dirname path) >>= fun () ->
  Log.info (sprintf "Write  : %S" path)   >>= fun () ->
  Writer.save path ~contents:data         >>= fun () ->
  Pipe.write dst info

let create ~dst ~riak_conn ~riak_bucket () =
  let t = {riak_conn; dst} in
  Log.info "Worker \"fetcher\" STARTED"                         >>= fun () ->
  Log.info (sprintf "Fetch  : keys of %s. Via 2i" riak_bucket)  >>= fun () ->
  Riak.Object.ID.fetch_via_2i riak_conn ~bucket:riak_bucket     >>= fun ids ->
  Deferred.List.iter ids ~how:`Parallel ~f:(fetch_object t)     >>= fun () ->
  Pipe.close dst;
  Log.info "Worker \"fetcher\" FINISHED"
