module String = StringLabels
module Unix   = UnixLabels

type process_error =
  | Fail   of int * string
  | Signal of int
  | Stop   of int

type argument_error =
  | Invalid_prog

type ('ok, 'error) result =
  [ `Ok of 'ok | `Error of 'error ]

type t =
  { prog   : string
  ; args   : string list
  ; stdout : in_channel
  ; stdin  : out_channel
  ; stderr : in_channel
  }

let read_ic ~ic =
  let buffer = Buffer.create 32 in
  let rec read () =
    Buffer.add_channel buffer ic 1;
    read ()
  in
  try read () with End_of_file -> ();
  Buffer.contents buffer

let string_find ~str ~chr =
  try Some (String.index str chr) with Not_found -> None

let wait {stdout; stdin; stderr; _} =
  let stdout_content = read_ic ~ic:stdout in
  let stderr_content = read_ic ~ic:stderr in
  match Unix.close_process_full (stdout, stdin, stderr) with
  | Unix.WEXITED   0 -> `Ok                stdout_content
  | Unix.WEXITED   n -> `Error (Fail   (n, stderr_content))
  | Unix.WSIGNALED n -> `Error (Signal  n)
  | Unix.WSTOPPED  n -> `Error (Stop    n)

let create ~prog ~args =
  match string_find ~str:prog ~chr:' ' with
  | Some _ -> `Error Invalid_prog
  | None ->
    let cmd = String.concat (prog :: args) ~sep:" " in
    let env = Unix.environment () in
    let stdout, stdin, stderr = Unix.open_process_full cmd ~env in
    let t =
      { prog
      ; args
      ; stdout
      ; stdin
      ; stderr
      }
    in
    `Ok t

let execute ~prog ~args =
  match create ~prog ~args with
  | `Error e -> `Error (`Create e)
  | `Ok    t ->
    match wait t with
    | `Error e    -> `Error (`Wait e)
    | `Ok    data -> `Ok data
