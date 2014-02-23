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
      match Process.create ~prog ~args with
      | `Error Process.Invalid_prog -> assert false
      | `Ok proc ->
        begin match Process.wait proc with
        | `Ok out  -> out
        | `Error (Process.Signal _) -> assert false
        | `Error (Process.Stop   _) -> assert false
        | `Error (Process.Fail (code, reason)) ->
            eprintf "~~~ FAILURE ~~~\n%!";
            eprintf "Program   : %s\n%!" prog;
            eprintf "Arguments : %s\n%!" (String.concat args ~sep:" ");
            eprintf "Exit code : %d\n%!" code;
            eprintf "Reason    : %s\n%!" reason;
            exit code
        end

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

module SnapsDB :
  sig
    val init : unit -> unit
    val put
       : bucket:string
      -> string * string  (* Key/Value pair. Unlabled for partial application. *)
      -> unit
  end
  =
  struct
    let init () =
      Cmd.exe ~prog:"git" ~args:["init"]

    let put ~bucket (key, value) =
      let path = bucket ^ "/" ^ key in
      let oc = open_out path in
      output_string oc value;
      close_out oc;
      Cmd.exe ~prog:"git" ~args:["add"; path];
      let status = Cmd.out ~prog:"git" ~args:["status"; "--porcelain"; path] in
      match status with
      | s when (s = "M  " ^ path ^ "\n") || (s = "A  " ^ path ^ "\n") ->
          eprintf "Committing %S. Status was: %S\n%!" path s;
          Cmd.exe ~prog:"git" ~args:["commit"; "-m"; sprintf "'Update %s'" path]
      | s ->
          eprintf "Not committing %S. Status was: %S\n%!" path s
  end

let () =
  let repo_path, hostname, bucket =
    try
      Sys.argv.(1), Sys.argv.(2), Sys.argv.(3)
    with Invalid_argument "index out of bounds" ->
      eprintf "USAGE: %s repo_path hostname bucket\n%!" Sys.argv.(0);
      exit 1
  in
  Cmd.exe ~prog:"mkdir" ~args:["-p"; (repo_path ^ "/" ^ bucket)];
  Sys.chdir repo_path;
  SnapsDB.init ();
  List.iter
    (Riak.fetch_value ~hostname ~bucket |- SnapsDB.put ~bucket)
    (Riak.fetch_keys ~hostname ~bucket)
