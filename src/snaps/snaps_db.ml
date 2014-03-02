open Core.Std
open Async.Std

module Ash = Async_shell

type t =
  { path : string
  ; commits_since_last_gc : int ref
  }

let create ~path =
  Ash.mkdir ~p:() path >>= fun () ->
  Sys.chdir path       >>= fun () ->
  Git.init ()          >>= fun () ->
  Sys.getcwd ()        >>= fun path ->  (* Remember the absolute path *)
  let t =
    { path
    ; commits_since_last_gc = ref 0
    }
  in
  return t

let gc t =
  Log.Global.info "GC BEGIN";
  Log.Global.flushed () >>= fun () ->
  Sys.chdir t.path      >>= fun () ->
  Git.gc ()             >>= fun () ->
  t.commits_since_last_gc := 0;
  Log.Global.info "GC END";
  Log.Global.flushed ()

let maybe_gc t =
  match !(t.commits_since_last_gc) with
  | 10 -> gc t
  | _  -> return ()

let put t ~bucket (key, value) =
  Sys.chdir t.path                    >>= fun () ->
  maybe_gc t                          >>= fun () ->
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
    Git.commit ~msg:(sprintf "'Add %s'" filepath) >>= fun () ->
    incr t.commits_since_last_gc;
    return ()

  | Git.Modified ->
    Log.Global.info "Commit : %S. Known status: Modified" filepath;
    Log.Global.flushed () >>= fun () ->
    Git.commit ~msg:(sprintf "'Update %s'" filepath) >>= fun () ->
    incr t.commits_since_last_gc;
    return ()

  | Git.Unchanged ->
    Log.Global.info "Skip   : %S. Known status: Unchanged" filepath;
    Log.Global.flushed ()

  | Git.Unexpected status ->
    Log.Global.info "Skip   : %S. Unknown status: %S" filepath status;
    Log.Global.flushed ()
