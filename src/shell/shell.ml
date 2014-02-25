open Printf
open StdLabels

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

let mkdir path =
  exe ~prog:"mkdir" ~args:["-p"; path]

let cd path =
  Sys.chdir path

let pwd () =
  Sys.getcwd ()
