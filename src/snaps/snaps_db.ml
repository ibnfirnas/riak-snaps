open Core.Std
open Async.Std

module Ash = Async_shell

type t =
  { path : string
  }

let create ~path =
  Ash.mkdir ~p:() path >>= fun () ->
  Sys.chdir path       >>= fun () ->
  Git.init ()          >>= fun () ->
  Sys.getcwd ()        >>= fun path ->  (* Remember the absolute path *)
  return {path}

let put {path} ~bucket (key, value) =
  Sys.chdir path                      >>= fun () ->
  Ash.mkdir ~p:() bucket              >>= fun () ->
  let filepath = Filename.concat bucket key in
  Log.Global.info "Write  : %S" filepath;
  Log.Global.flushed ()                >>= fun () ->
  Writer.save filepath ~contents:value >>= fun () ->
  Git.add ~filepath                    >>= fun () ->
  Git.status ~filepath >>= function
  | Git.Added ->
    Log.Global.info "Commit : %S. Known status: Added" filepath;
    Log.Global.flushed () >>= fun () ->
    Git.commit ~msg:(sprintf "'Add %s'" filepath)

  | Git.Modified ->
    Log.Global.info "Commit : %S. Known status: Modified" filepath;
    Log.Global.flushed () >>= fun () ->
    Git.commit ~msg:(sprintf "'Update %s'" filepath)

  | Git.Unchanged ->
    Log.Global.info "Skip   : %S. Known status: Unchanged" filepath;
    Log.Global.flushed ()

  | Git.Unexpected status ->
    Log.Global.info "Skip   : %S. Unknown status: %S" filepath status;
    Log.Global.flushed ()
