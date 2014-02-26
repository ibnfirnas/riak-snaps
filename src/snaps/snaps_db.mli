type t

val create : path:string -> t

val put
   : t
  -> bucket:string
  -> string * string  (* Key/Value pair. Unlabled for partial application. *)
  -> unit
