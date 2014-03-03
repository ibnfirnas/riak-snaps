open Core.Std
open Async.Std

module Log = Snaps_log

type t = { db     : Snaps_db.t
         ; src    : Riak.Object.t Pipe.Reader.t
         }

let rec store t =
  let {src; db} = t in
  Pipe.read src >>= function
  | `Eof   ->
    Pipe.close_read src;
    return ()

  | `Ok obj ->
    Snaps_db.put db obj >>= fun () ->
    store t

let create ~src ~db () =
  Log.info "Worker \"storer\" STARTED" >>= fun () ->
  let t = {src; db} in
  store t >>= fun () ->
  Log.info "Worker \"storer\" FINISHED"
