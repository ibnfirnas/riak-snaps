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

let of_char = function
  | '?' -> Untracked
  | ' ' -> Unmodified
  | 'M' -> Modified
  | 'A' -> Added
  | 'D' -> Deleted
  | 'R' -> Renamed
  | 'C' -> Copied
  | 'U' -> Updated_unmerged
  | _   -> assert false
