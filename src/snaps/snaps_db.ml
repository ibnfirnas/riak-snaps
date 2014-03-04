open Core.Std
open Async.Std

module Ash = Async_shell
module Log = Snaps_log.Make (struct let name = "Snaps_db" end)

type t =
  { path : string
  ; commits_before_gc_minor     : int
  ; commits_before_gc_major     : int
  ; commits_since_last_gc_minor : int ref
  ; commits_since_last_gc_major : int ref
  }

let create ~path ~commits_before_gc_minor ~commits_before_gc_major =
  Ash.mkdir ~p:() path >>= fun () ->
  Sys.chdir path       >>= fun () ->
  Git.init ()          >>= fun () ->
  Sys.getcwd ()        >>| fun path ->  (* Remember the absolute path *)
  { path
  ; commits_before_gc_minor
  ; commits_before_gc_major
  ; commits_since_last_gc_minor = ref 0
  ; commits_since_last_gc_major = ref 0
  }

let gc_minor t =
  Log.info "GC minor BEGIN"   >>= fun () ->
  Sys.chdir t.path            >>= fun () ->
  Git.gc ~aggressive:false () >>= fun () ->
  t.commits_since_last_gc_minor := 0;
  Log.info "GC minor END"

let gc_major t =
  Log.info "GC major BEGIN"  >>= fun () ->
  Sys.chdir t.path           >>= fun () ->
  Git.gc ~aggressive:true () >>= fun () ->
  t.commits_since_last_gc_major := 0;
  Log.info "GC major END"

let maybe_gc t =
  if !(t.commits_since_last_gc_major) >= t.commits_before_gc_major
  then gc_major t
  else if !(t.commits_since_last_gc_minor) >= t.commits_before_gc_minor
       then gc_minor t
       else return ()

let put t obj_info =
  let path_to_data = Snaps_object_info.path_to_data obj_info in
  Sys.chdir t.path                    >>= fun () ->
  maybe_gc t                          >>= fun () ->
  let p = path_to_data in
  Git.add ~filepath:p                 >>= fun () ->
  Git.status ~filepath:p >>= function
  | Git.Added ->
    Log.info (sprintf "Commit BEGIN: %S. Known status: Added" p) >>= fun () ->
    Git.commit ~msg:(sprintf "'Add %s'" p)                       >>= fun () ->
    Log.info (sprintf "Commit END: %S. Known status: Added" p)   >>| fun () ->
    incr t.commits_since_last_gc_minor;
    incr t.commits_since_last_gc_major

  | Git.Modified ->
    Log.info (sprintf "Commit BEGIN: %S. Known status: Modified" p) >>= fun () ->
    Git.commit ~msg:(sprintf "'Update %s'" p)                       >>= fun () ->
    Log.info (sprintf "Commit END: %S. Known status: Modified" p)   >>| fun () ->
    incr t.commits_since_last_gc_minor;
    incr t.commits_since_last_gc_major

  | Git.Unchanged ->
    Log.info (sprintf "Skip: %S. Known status: Unchanged" p)

  | Git.Unexpected status ->
    Log.info (sprintf "Skip: %S. Unknown status: %S" p status)
