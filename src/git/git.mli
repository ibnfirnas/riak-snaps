type status = Unchanged
            | Added
            | Modified
            | Unexpected of string

val init   :            unit -> unit

val add    : filepath:string -> unit

val status : filepath:string -> status

val commit :      msg:string -> unit
