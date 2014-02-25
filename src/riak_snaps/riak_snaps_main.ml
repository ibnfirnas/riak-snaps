open Printf
open Riak_snaps_pervasives

module String = StringLabels

module Cmd :
  sig
    val out : prog:string -> args:string list -> string
    val exe : prog:string -> args:string list -> unit
  end
  =
  struct
    let out ~prog ~args =
      match Process.execute ~prog ~args with
      | `Ok output                            -> output
      | `Error (`Create Process.Invalid_prog) -> assert false
      | `Error (`Wait  (Process.Signal _))    -> assert false
      | `Error (`Wait  (Process.Stop   _))    -> assert false
      | `Error (`Wait  (Process.Fail (code, reason))) ->
          eprintf "~~~ FAILURE ~~~\n%!";
          eprintf "Program   : %s\n%!" prog;
          eprintf "Arguments : %s\n%!" (String.concat args ~sep:" ");
          eprintf "Exit code : %d\n%!" code;
          eprintf "Reason    : %s\n%!" reason;
          exit code

    let exe ~prog ~args =
      ignore (out ~prog ~args)
  end

module Riak :
  sig
    val fetch_keys
       : hostname:string
      -> bucket:string
      -> string list

    val fetch_value
       : hostname:string
      -> bucket:string
      -> string  (* Key. Unlabled for partial application. *)
      -> string * string
  end
  =
  struct
    let port = 8098

    let fetch_keys ~hostname ~bucket =
      let uri = sprintf "http://%s:%d/riak/%s?keys=true" hostname port bucket in
      let data = Cmd.out ~prog:"curl" ~args:[uri] in
      let json = Ezjsonm.from_string data in
      Ezjsonm.(get_list get_string (find json ["keys"]))

    let fetch_value ~hostname ~bucket key =
      let uri = sprintf "http://%s:%d/riak/%s/%s" hostname port bucket key in
      let value = Cmd.out ~prog:"curl" ~args:[uri] in
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
      Cmd.exe ~prog:"git" ~args:["init"]

    let add ~filepath =
      Cmd.exe ~prog:"git" ~args:["add"; filepath]

    let status ~filepath =
      match Cmd.out ~prog:"git" ~args:["status"; "--porcelain"; filepath] with
      | ""                                   -> Unchanged
      | s when (s = "A  " ^ filepath ^ "\n") -> Added
      | s when (s = "M  " ^ filepath ^ "\n") -> Modified
      | s                                    -> assert false
      (* TODO: Handle other status codes. *)

    let commit ~msg =
      Cmd.exe ~prog:"git" ~args:["commit"; "-m"; msg]
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
      Cmd.exe ~prog:"mkdir" ~args:["-p"; path]

    let create ~path =
      mkdir path;
      Sys.chdir path;
      Git.init ();
      { path = Sys.getcwd ()  (* Remember the absolute path *)
      }

    let put {path} ~bucket (key, value) =
      Sys.chdir path;
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
  List.iter
    (Riak.fetch_value ~hostname ~bucket |- SnapsDB.put db ~bucket)
    (Riak.fetch_keys ~hostname ~bucket)
