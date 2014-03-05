open Core.Std
open Async.Std

module Ash = Async_shell
module Log = Snaps_log.Make (struct let name = "Snaps_db" end)

type t =
  { path : string
  ; updates_channel             : Snaps_work_progress.update_msg Pipe.Writer.t
  ; commits_before_gc_minor     : int
  ; commits_before_gc_major     : int
  ; commits_since_last_gc_minor : int ref
  ; commits_since_last_gc_major : int ref
  }

let create ~path ~updates_channel ~commits_before_gc_minor ~commits_before_gc_major =
  Ash.mkdir ~p:() path >>= fun () ->
  Sys.chdir path       >>= fun () ->
  Git.init ()          >>= fun () ->
  Sys.getcwd ()        >>| fun path ->  (* Remember the absolute path *)
  { path
  ; updates_channel
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

let handle_git_error {path} = function
  | Git.Unexpected_stderr stderr ->
    Log.error (sprintf "Git.Unexpected_stderr %S" stderr) >>= fun () ->
    assert false

  | Git.Unable_to_create_file filepath ->
    Log.error (sprintf "Git.Unable_to_create_file %S" filepath) >>= fun () ->
    if filepath = (Filename.concat path ".git/index.lock") then
      Log.info (sprintf "Removing expected lockfile: %S" filepath) >>= fun () ->
      Sys.remove filepath
    else
      let msg = sprintf
        "Don't know what to do when Git cannot create this file: %S!" filepath
      in
      Log.error msg >>= fun () ->
      assert false

let put t obj_info =
  let path_to_data = Snaps_object_info.path_to_data obj_info in
  Sys.chdir t.path >>= fun () ->
  maybe_gc t       >>= fun () ->
  let p = path_to_data in
  Git.add ~filepath:p
  >>= ( function
      | Ok ()   -> return ()
      | Error e -> handle_git_error t e >>= fun () ->
                   Git.add_exn ~filepath:p
      )
  >>= fun () ->
  Git.status ~filepath:p
  >>= begin function
  | Git.Unexpected s -> begin
      Log.info (sprintf "Skip: %S. Unknown status: %S" p s) >>= fun () ->
      Pipe.write t.updates_channel `Skipped
    end
  | Git.Unchanged -> begin
      Log.info (sprintf "Skip: %S. Known status: Unchanged" p) >>= fun () ->
      Pipe.write t.updates_channel `Skipped
    end
  | Git.Added -> begin
      Log.info (sprintf "Commit BEGIN: %S. Known status: Added" p) >>= fun () ->
      let msg = sprintf "'Add %s'" p in
      Git.commit ~msg
      >>= ( function
          | Ok ()   -> return ()
          | Error e -> handle_git_error t e >>= fun () ->
                       Git.commit_exn ~msg
          )
      >>= fun () ->
      Log.info (sprintf "Commit END: %S. Known status: Added" p) >>= fun () ->
      incr t.commits_since_last_gc_minor;
      incr t.commits_since_last_gc_major;
      Pipe.write t.updates_channel `Committed
    end
  | Git.Modified -> begin
      Log.info (sprintf "Commit BEGIN: %S. Known status: Modified" p)
      >>= fun () ->
      let msg = sprintf "'Update %s'" p in
      Git.commit ~msg
      >>= ( function
          | Ok ()   -> return ()
          | Error e -> handle_git_error t e >>= fun () ->
                       Git.commit_exn ~msg
          )
      >>= fun () ->
      Log.info (sprintf "Commit END: %S. Known status: Modified" p)
      >>= fun () ->
      incr t.commits_since_last_gc_minor;
      incr t.commits_since_last_gc_major;
      Pipe.write t.updates_channel `Committed
    end
  end
