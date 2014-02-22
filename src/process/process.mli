type process_error =
  | Fail   of int * string
  | Signal of int
  | Stop   of int

type argument_error =
  | Invalid_prog

type ('ok, 'error) result =
  [ `Ok of 'ok | `Error of 'error ]

type t

val create : prog:string -> args:string list -> (t, argument_error) result

val wait : t -> (string, process_error) result
