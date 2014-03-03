open Core.Std
open Async.Std

module Ash = Async_shell
module Log = Snaps_log

type t =
  { path : string
  ; commits_before_gc     : int
  ; commits_since_last_gc : int ref
  }

let create ~path ~commits_before_gc =
  Ash.mkdir ~p:() path >>= fun () ->
  Sys.chdir path       >>= fun () ->
  Git.init ()          >>= fun () ->
  Sys.getcwd ()        >>| fun path ->  (* Remember the absolute path *)
  { path
  ; commits_before_gc
  ; commits_since_last_gc = ref 0
  }

let gc t =
  Log.info "GC BEGIN"   >>= fun () ->
  Sys.chdir t.path      >>= fun () ->
  Git.gc ()             >>= fun () ->
  t.commits_since_last_gc := 0;
  Log.info "GC END"

let maybe_gc t =
  if !(t.commits_since_last_gc) >= t.commits_before_gc
  then gc t
  else return ()

let put t obj_info =
  let path_to_data = Snaps_object_info.path_to_data obj_info in
  Sys.chdir t.path                    >>= fun () ->
  maybe_gc t                          >>= fun () ->
  let p = path_to_data in
  Git.add ~filepath:p                 >>= fun () ->
  Git.status ~filepath:p >>= function
  | Git.Added ->
    Log.info (sprintf "Commit : %S. Known status: Added" p) >>= fun () ->
    Git.commit ~msg:(sprintf "'Add %s'" p) >>| fun () ->
    incr t.commits_since_last_gc

  | Git.Modified ->
    Log.info (sprintf "Commit : %S. Known status: Modified" p) >>= fun () ->
    Git.commit ~msg:(sprintf "'Update %s'" p) >>| fun () ->
    incr t.commits_since_last_gc

  | Git.Unchanged ->
    Log.info (sprintf "Skip   : %S. Known status: Unchanged" p)

  | Git.Unexpected status ->
    Log.info (sprintf "Skip   : %S. Unknown status: %S" p status)
