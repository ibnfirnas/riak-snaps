open Core.Std
open Async.Std

module Log = Snaps_log

type t = { db     : Snaps_db.t
         ; src    : Snaps_object_info.t Pipe.Reader.t
         }

let rec store t =
  let {src; db} = t in
  Pipe.read src >>= function
  | `Eof   ->
    Pipe.close_read src;
    return ()

  | `Ok object_info ->
    Snaps_db.put db object_info >>= fun () ->
    store t

let create ~src ~db () =
  Log.info "Worker \"storer\" STARTED" >>= fun () ->
  let t = {src; db} in
  store t >>= fun () ->
  Log.info "Worker \"storer\" FINISHED"
