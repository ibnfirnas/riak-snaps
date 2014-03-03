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

let put t obj =
  let obj_id     = obj.Riak.Object.id in
  let obj_bucket = obj_id.Riak.Object.ID.bucket in
  let obj_key    = obj_id.Riak.Object.ID.key in
  let obj_data   = obj.Riak.Object.data in
  Sys.chdir t.path                    >>= fun () ->
  maybe_gc t                          >>= fun () ->
  Ash.mkdir ~p:() obj_bucket          >>= fun () ->
  let filepath = Filename.concat obj_bucket obj_key in
  Log.info (sprintf "Write  : %S" filepath) >>= fun () ->
  Writer.save filepath ~contents:obj_data   >>= fun () ->
  Git.add ~filepath                    >>= fun () ->
  Git.status ~filepath >>= function
  | Git.Added ->
    Log.info (sprintf "Commit : %S. Known status: Added" filepath) >>= fun () ->
    Git.commit ~msg:(sprintf "'Add %s'" filepath) >>| fun () ->
    incr t.commits_since_last_gc

  | Git.Modified ->
    Log.info (sprintf "Commit : %S. Known status: Modified" filepath) >>= fun () ->
    Git.commit ~msg:(sprintf "'Update %s'" filepath) >>| fun () ->
    incr t.commits_since_last_gc

  | Git.Unchanged ->
    Log.info (sprintf "Skip   : %S. Known status: Unchanged" filepath)

  | Git.Unexpected status ->
    Log.info (sprintf "Skip   : %S. Unknown status: %S" filepath status)
