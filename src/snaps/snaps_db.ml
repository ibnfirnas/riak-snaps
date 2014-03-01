open Core.Std
open Async.Std

type t =
  { path : string
  }

let create ~path =
  Shell.mkdir path;
  Shell.cd path;
  Git.init ();
  { path = Shell.pwd ()  (* Remember the absolute path *)
  }

let put {path} ~bucket (key, value) =
  Shell.cd path;
  Shell.mkdir bucket;
  let filepath = Filename.concat bucket key in
  eprintf "Write  : %S\n%!" filepath;
  Out_channel.write_all filepath ~data:value;
  Git.add ~filepath;
  begin match Git.status ~filepath with
  | Git.Added ->
    eprintf "Commit : %S. Known status: Added\n%!" filepath;
    Git.commit ~msg:(sprintf "'Add %s'" filepath)

  | Git.Modified ->
    eprintf "Commit : %S. Known status: Modified\n%!" filepath;
    Git.commit ~msg:(sprintf "'Update %s'" filepath)

  | Git.Unchanged ->
    eprintf "Skip   : %S. Known status: Unchanged\n%!" filepath

  | Git.Unexpected status ->
    eprintf "Skip   : %S. Unknown status: %S\n%!" filepath status
  end;
  return ()
