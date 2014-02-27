open Printf

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
  let filepath = bucket ^ "/" ^ key in
  let oc = open_out filepath in
  output_string oc value;
  close_out oc;
  Git.add ~filepath;
  match Git.status ~filepath with
  | Git.Added ->
    eprintf "Committing: %S. Known status: Added\n%!" filepath;
    Git.commit ~msg:(sprintf "'Add %s'" filepath)

  | Git.Modified ->
    eprintf "Committing: %S. Known status: Modified\n%!" filepath;
    Git.commit ~msg:(sprintf "'Update %s'" filepath)

  | Git.Unchanged ->
    eprintf "Skipping: %S. Known status: Unchanged\n%!" filepath

  | Git.Unexpected status ->
    eprintf "Skipping: %S. Unknown status: %S\n%!" filepath status
