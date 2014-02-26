open Printf

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
