type status = Unchanged
            | Added
            | Modified
            | Unexpected of string

let init () =
  Shell.exe ~prog:"git" ~args:["init"]

let add ~filepath =
  Shell.exe ~prog:"git" ~args:["add"; filepath]

let status ~filepath =
  match Shell.out ~prog:"git" ~args:["status"; "--porcelain"; filepath] with
  | ""                                   -> Unchanged
  | s when (s = "A  " ^ filepath ^ "\n") -> Added
  | s when (s = "M  " ^ filepath ^ "\n") -> Modified
  | s                                    -> Unexpected s
  (* TODO: Handle other status codes. *)

let commit ~msg =
  Shell.exe ~prog:"git" ~args:["commit"; "-m"; msg]
