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
  Log.Global.info "Write  : %S" filepath;
  Out_channel.write_all filepath ~data:value;
  Git.add ~filepath;
  begin match Git.status ~filepath with
  | Git.Added ->
    Log.Global.info "Commit : %S. Known status: Added" filepath;
    Git.commit ~msg:(sprintf "'Add %s'" filepath)

  | Git.Modified ->
    Log.Global.info "Commit : %S. Known status: Modified" filepath;
    Git.commit ~msg:(sprintf "'Update %s'" filepath)

  | Git.Unchanged ->
    Log.Global.info "Skip   : %S. Known status: Unchanged" filepath

  | Git.Unexpected status ->
    Log.Global.info "Skip   : %S. Unknown status: %S" filepath status
  end;
  return ()
