open Printf
open Riak_snaps_pervasives

module String = StringLabels

let port = 8098

let cmd_out ~prog ~args =
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

let cmd_do ~prog ~args =
  ignore (cmd_out ~prog ~args)

let riak_fetch_keys ~hostname ~bucket =
  let uri = sprintf "http://%s:%d/riak/%s?keys=true" hostname port bucket in
  let data = cmd_out ~prog:"curl" ~args:[uri] in
  let json = Ezjsonm.from_string data in
  Ezjsonm.(get_list get_string (find json ["keys"]))

let riak_fetch_value ~hostname ~bucket key =
  let uri = sprintf "http://%s:%d/riak/%s/%s" hostname port bucket key in
  let value = cmd_out ~prog:"curl" ~args:[uri] in
  key, value

let git_init () =
  cmd_do ~prog:"git" ~args:["init"]

let mkdir path =
  cmd_do ~prog:"mkdir" ~args:["-p"; path]

let object_store ~bucket (key, value) =
  let path = bucket ^ "/" ^ key in
  let oc = open_out path in
  output_string oc value;
  close_out oc;
  cmd_do ~prog:"git" ~args:["add"; path];
  let status = cmd_out ~prog:"git" ~args:["status"; "--porcelain"; path] in
  match status with
  | s when (s = "M  " ^ path ^ "\n") || (s = "A  " ^ path ^ "\n") ->
      eprintf "Committing %S. Status was: %S\n%!" path s;
      cmd_do ~prog:"git" ~args:["commit"; "-m"; sprintf "'Update %s'" path]
  | s ->
      eprintf "Not committing %S. Status was: %S\n%!" path s

let () =
  let repo_path = Sys.argv.(1) in
  let hostname  = Sys.argv.(2) in
  let bucket    = Sys.argv.(3) in
  mkdir (repo_path ^ "/" ^ bucket);
  Sys.chdir repo_path;
  git_init ();
  List.iter
    (riak_fetch_value ~hostname ~bucket |- object_store ~bucket)
    (riak_fetch_keys ~hostname ~bucket)
