open Core.Std
open Async.Std

module Term : sig
  val clear : unit -> unit
  (** Clear screen *)

  val reset : unit -> unit
  (** Reset cursor position *)
end = struct
  let ansi_code_clear =
    "\027[2J"

  let ansi_code_reset =
    "\027[1;1H"

  let clear () =
    print_string ansi_code_clear

  let reset () =
    print_string ansi_code_reset
end

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
  Term.reset ();
  print_endline (String.concat lines ~sep:"\n");
  return ()

let rec read t ~updates_channel =
  Pipe.read updates_channel
  >>= function
    | `Eof ->
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
  Term.clear ();
  read t ~updates_channel
