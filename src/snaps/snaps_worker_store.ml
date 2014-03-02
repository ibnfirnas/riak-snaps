open Core.Std
open Async.Std

module Log = Snaps_log

type t = { db     : Snaps_db.t
         ; bucket : string
         ; src    : (string * string) Pipe.Reader.t
         }

let rec store t =
  let {src; db; bucket} = t in
  Pipe.read src >>= function
  | `Eof   ->
    Pipe.close_read src;
    return ()

  | `Ok kv ->
    Snaps_db.put db ~bucket kv >>= fun () ->
    store t

let create ~src ~db ~bucket () =
  Log.info "Worker \"storer\" STARTED" >>= fun () ->
  let t = {src; db; bucket} in
  store t >>= fun () ->
  Log.info "Worker \"storer\" FINISHED"
