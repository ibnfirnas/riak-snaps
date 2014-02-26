type status = Unchanged
            | Added
            | Modified

val init   :            unit -> unit

val add    : filepath:string -> unit

val status : filepath:string -> status

val commit :      msg:string -> unit
