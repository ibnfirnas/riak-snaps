open Core.Std
open Async.Std

module IO : sig
  val fetch : uri:string -> string Deferred.t
end = struct
  let fetch ~uri =
    Async_shell.run_full "curl" [uri]
end

module Conn : sig
  type t =
    { hostname : string
    ; port     : int
    }

  val make : ?hostname:string -> ?port:int -> unit -> t
end = struct
  type t =
    { hostname : string
    ; port     : int
    }

  let make ?(hostname="localhost") ?(port=8098) () =
    { hostname
    ; port
    }
end

module Object = struct
  module ID = struct
    type t =
      { bucket : string
      ; key    : string
      }

    let fetch ~uri ~bucket =
      IO.fetch ~uri >>| fun data ->
      let json = Ezjsonm.from_string data in
      let keys = Ezjsonm.(get_list get_string (find json ["keys"])) in
      List.map keys ~f:(fun key -> {bucket; key})

    let fetch_via_2i {Conn.hostname; Conn.port} ~bucket =
      let uri =
        sprintf
          "http://%s:%d/buckets/%s/index/$bucket/_"
          hostname
          port
          bucket
      in
      fetch ~uri ~bucket

    let fetch_via_brute_force {Conn.hostname; Conn.port} ~bucket =
      let uri = sprintf "http://%s:%d/riak/%s?keys=true" hostname port bucket in
      fetch ~uri ~bucket

    let to_string {bucket; key} =
      bucket ^ "/" ^ key
  end

  type t = { id   : ID.t
           ; data : string
           }

  let fetch {Conn.hostname; Conn.port} id =
    let uri =
      sprintf
        "http://%s:%d/riak/%s/%s"
        hostname
        port
        id.ID.bucket
        id.ID.key
    in
    IO.fetch ~uri >>| fun data ->
    {id; data}
end
