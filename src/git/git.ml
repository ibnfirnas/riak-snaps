open Core.Std
open Async.Std

type status = Unchanged
            | Added
            | Modified
            | Unexpected of string

let init () =
  Async_shell.run "git" ["init"]

let add ~filepath =
  Async_shell.run "git" ["add"; filepath]

let status ~filepath =
  Async_shell.run_full "git" ["status"; "--porcelain"; filepath] >>| function
  | ""                                   -> Unchanged
  | s when (s = "A  " ^ filepath ^ "\n") -> Added
  | s when (s = "M  " ^ filepath ^ "\n") -> Modified
  | s                                    -> (Unexpected s)
  (* TODO: Handle other status codes. *)

let commit ~msg =
  Async_shell.run "git" ["commit"; "-m"; msg]

let gc ?(aggressive=false) () =
  let aggressive_flag = if aggressive then ["--aggressive"] else [] in
  Async_shell.run "git" ("gc" :: "--prune=now" :: aggressive_flag)
