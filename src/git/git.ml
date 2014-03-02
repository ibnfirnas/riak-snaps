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
  Async_shell.run_full "git" ["status"; "--porcelain"; filepath] >>= function
  | ""                                   -> return Unchanged
  | s when (s = "A  " ^ filepath ^ "\n") -> return Added
  | s when (s = "M  " ^ filepath ^ "\n") -> return Modified
  | s                                    -> return (Unexpected s)
  (* TODO: Handle other status codes. *)

let commit ~msg =
  Async_shell.run "git" ["commit"; "-m"; msg]

let gc () =
  Async_shell.run "git" ["gc"; "--prune=now"]
