open Core.Std
open Async.Std

module Log = Snaps_log

type t = { riak   : Riak.t
         ; bucket : string
         ; dst    : (string * string) Pipe.Writer.t
         }

let fetch_object {riak; bucket; dst} key =
  Log.info (sprintf "Fetch  : %S" (bucket ^ "/" ^ key)) >>= fun () ->
  Riak.fetch_value riak ~bucket key                     >>= fun kv ->
  Pipe.write dst kv

let create ~dst ~riak ~bucket () =
  let t = {riak; bucket; dst} in
  Log.info "Worker \"fetcher\" STARTED"                      >>= fun () ->
  Log.info (sprintf "Fetch  : keys of %s. Via 2i" bucket)    >>= fun () ->
  Riak.fetch_keys_2i riak ~bucket                            >>= fun keys ->
  Deferred.List.iter keys ~how:`Parallel ~f:(fetch_object t) >>= fun () ->
  Pipe.close dst;
  Log.info "Worker \"fetcher\" FINISHED"
