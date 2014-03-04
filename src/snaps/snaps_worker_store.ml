open Core.Std
open Async.Std

module Log = Snaps_log.Make (struct let name = "Snaps_worker_store" end)

type t = { db     : Snaps_db.t
         ; r      : Snaps_object_info.t Pipe.Reader.t
         }

let rec store t =
  let {r; db} = t in
  Pipe.read r >>= function
  | `Eof   ->
    Snaps_db.gc_major db >>| fun () ->
    Pipe.close_read r

  | `Ok object_info ->
    Snaps_db.put db object_info >>= fun () ->
    store t

let create ~r ~db () =
  Log.info "Worker STARTED" >>= fun () ->
  let t = {r; db} in
  store t >>= fun () ->
  Log.info "Worker FINISHED"
