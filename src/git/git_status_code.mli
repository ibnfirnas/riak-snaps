open Core.Std

type t =
  | Untracked
  | Unmodified
  | Modified
  | Added
  | Deleted
  | Renamed
  | Copied
  | Updated_unmerged

val of_char : char -> t
