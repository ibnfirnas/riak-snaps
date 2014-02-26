open Printf

type t =
  { hostname : string
  ; port     : int
  }

let make ?(hostname="localhost") ?(port=8098) () =
  { hostname
  ; port
  }

let fetch_keys {hostname; port} ~bucket =
  let uri = sprintf "http://%s:%d/riak/%s?keys=true" hostname port bucket in
  let data = Shell.out ~prog:"curl" ~args:[uri] in
  let json = Ezjsonm.from_string data in
  Ezjsonm.(get_list get_string (find json ["keys"]))

let fetch_value {hostname; port} ~bucket key =
  let uri = sprintf "http://%s:%d/riak/%s/%s" hostname port bucket key in
  let value = Shell.out ~prog:"curl" ~args:[uri] in
  key, value