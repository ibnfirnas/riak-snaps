type t

val make : ?hostname:string -> ?port:int -> unit -> t

val fetch_keys
   : t
  -> bucket:string
  -> string list

val fetch_value
   : t
  -> bucket:string
  -> string  (* Key. Unlabled for partial application. *)
  -> string * string
