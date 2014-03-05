open Core.Std
open Async.Std

type status = Unchanged
            | Added
            | Modified
            | Unexpected of string

type error = Unable_to_create_file of string
           | Unexpected_stderr     of string

let init () =
  Async_shell.run "git" ["init"]

let parse_stderr stderr =
  try
    let msg = List.hd_exn (Str.split (Str.regexp "\n+") stderr) in
    let f = Scanf.sscanf msg "fatal: Unable to create '%s@': File exists." Fn.id in
    Unable_to_create_file f
  with
  | Failure "hd" | Scanf.Scan_failure _ ->
    Unexpected_stderr stderr

let try_with_parse_stderr ~f =
  let module P = Async_shell.Process in
  try_with ~extract_exn:true f >>| function
  | Ok ok                       -> Ok ok
  | Error (P.Failed {P.stderr}) -> Error (parse_stderr stderr)
  | Error _                     -> assert false

let status ~filepath =
  Async_shell.run_full "git" ["status"; "--porcelain"; filepath] >>| function
  | ""                                   -> Unchanged
  | s when (s = "A  " ^ filepath ^ "\n") -> Added
  | s when (s = "M  " ^ filepath ^ "\n") -> Modified
  | s                                    -> (Unexpected s)
  (* TODO: Handle other status codes. *)

let add ~filepath =
  try_with_parse_stderr (fun () -> Async_shell.run "git" ["add"; filepath])

let commit ~msg =
  try_with_parse_stderr (fun () -> Async_shell.run "git" ["commit"; "-m"; msg])

let gc ?(aggressive=false) () =
  let aggressive_flag = if aggressive then ["--aggressive"] else [] in
  Async_shell.run "git" ("gc" :: "--prune=now" :: aggressive_flag)
