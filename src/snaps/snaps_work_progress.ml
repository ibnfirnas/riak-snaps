open Core.Std
open Async.Std

type update_msg  = [ `Fetched | `Committed | `Skipped ]

type t =
  { total     : int
  ; fetched   : int
  ; committed : int
  ; skipped   : int
  }

let print t =
  let lines =
    [ sprintf "Total     : %d" t.total
    ; sprintf "Fetched   : %d" t.fetched
    ; sprintf "Committed : %d" t.committed
    ; sprintf "Skipped   : %d" t.skipped
    ]
  in
  print_endline (String.concat lines ~sep:"\n");
  return ()

let rec read t ~updates_channel =
  Pipe.read updates_channel
  >>= function
    | `Eof ->
      Pipe.close_read updates_channel;
      return ()

    | `Ok `Fetched ->
      let t = {t with fetched = succ t.fetched} in
      print t >>= fun () ->
      read t ~updates_channel

    | `Ok `Committed ->
      let t = {t with committed = succ t.committed} in
      print t >>= fun () ->
      read t ~updates_channel

    | `Ok `Skipped ->
      let t = {t with skipped = succ t.skipped} in
      print t >>= fun () ->
      read t ~updates_channel

let run ~total_objects ~updates_channel () =
  let t =
    { total     = total_objects
    ; fetched   = 0
    ; committed = 0
    ; skipped   = 0
    }
  in
  read t ~updates_channel
