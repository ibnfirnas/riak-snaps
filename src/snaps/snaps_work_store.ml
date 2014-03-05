open Core.Std
open Async.Std

module Log = Snaps_log.Make (struct let name = "Snaps_work_store" end)

type t = { db     : Snaps_db.t
         ; r      : Snaps_object_info.t Pipe.Reader.t
         ; updates_channel : Snaps_work_progress.update_msg Pipe.Writer.t
         }

let rec store t =
  let {r; db; updates_channel} = t in
  Pipe.read r
  >>= function
    | `Eof -> begin
      Snaps_db.gc_major db >>| fun () ->
      Pipe.close_read r;
      Pipe.close updates_channel
    end
    | `Ok object_info -> begin
      Snaps_db.put db object_info >>= fun () ->
      store t
    end

let run ~r ~db ~updates_channel () =
  Log.info "Worker STARTED" >>= fun () ->
  let t = {r; db; updates_channel} in
  store t >>= fun () ->
  Log.info "Worker FINISHED"
