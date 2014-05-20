open Core.Std

type t =
  | Normal of Git_status_code.t * Git_status_code.t * string
  | Rename of Git_status_code.t * Git_status_code.t * string * string
