open Printf
open Snaps_pervasives

module Riak :
  sig
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
  end
  =
  struct
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
  end

module Git :
  sig
    type status = Unchanged
                | Added
                | Modified

    val init   :            unit -> unit
    val add    : filepath:string -> unit
    val status : filepath:string -> status
    val commit :      msg:string -> unit
  end
  =
  struct
    type status = Unchanged
                | Added
                | Modified

    let init () =
      Shell.exe ~prog:"git" ~args:["init"]

    let add ~filepath =
      Shell.exe ~prog:"git" ~args:["add"; filepath]

    let status ~filepath =
      match Shell.out ~prog:"git" ~args:["status"; "--porcelain"; filepath] with
      | ""                                   -> Unchanged
      | s when (s = "A  " ^ filepath ^ "\n") -> Added
      | s when (s = "M  " ^ filepath ^ "\n") -> Modified
      | s                                    -> assert false
      (* TODO: Handle other status codes. *)

    let commit ~msg =
      Shell.exe ~prog:"git" ~args:["commit"; "-m"; msg]
  end

module SnapsDB :
  sig
    type t

    val create : path:string -> t

    val put
       : t
      -> bucket:string
      -> string * string  (* Key/Value pair. Unlabled for partial application. *)
      -> unit
  end
  =
  struct
    type t =
      { path : string
      }

    let mkdir path =
      Shell.exe ~prog:"mkdir" ~args:["-p"; path]

    let create ~path =
      mkdir path;
      Shell.cd path;
      Git.init ();
      { path = Shell.pwd ()  (* Remember the absolute path *)
      }

    let put {path} ~bucket (key, value) =
      Shell.cd path;
      mkdir bucket;
      let filepath = bucket ^ "/" ^ key in
      let oc = open_out filepath in
      output_string oc value;
      close_out oc;
      Git.add ~filepath;
      match Git.status ~filepath with
      | Git.Added | Git.Modified ->
          eprintf "Committing %S. Status was Added or Modified.\n%!" filepath;
          Git.commit ~msg:(sprintf "'Update %s'" filepath)
      | Git.Unchanged ->
          eprintf "Not committing %S. Status was Unchanged.\n%!" filepath
  end

let () =
  let repo_path, hostname, bucket =
    try
      Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)
    with Invalid_argument "index out of bounds" ->
      eprintf "USAGE: %s repo_path hostname bucket\n%!" Sys.argv.(0);
      exit 1
  in
  let db = SnapsDB.create ~path:repo_path in
  let riak = Riak.make ~hostname () in
  List.iter
    (Riak.fetch_value riak ~bucket |- SnapsDB.put db ~bucket)
    (Riak.fetch_keys  riak ~bucket)
