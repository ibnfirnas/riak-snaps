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

type simple_git_status =
  | Added
  | Modified
  | Unchanged
  | Unexpected of Git_status.t


let simple_git_status s =
  let module GS  = Git_status in
  let module GSC = Git_status_code in
  match s with
  | GS.Normal (GSC.Added      , _, _) -> Added
  | GS.Normal (GSC.Modified   , _, _) -> Modified
  | GS.Normal (GSC.Unmodified , _, _) -> Unchanged
  | GS.Normal _
  | GS.Rename _                       -> Unexpected s

let list_group_by l ~f : ('k * ('v list)) list =
  List.fold_left l ~init:[] ~f:(
    fun groups value ->
      let key = f value in
      let group =
        match List.Assoc.find groups key with
        | Some values -> value :: values
        | None        -> [value]
      in
      List.Assoc.add groups key group
  )

let handle_git_error = function
  | Git.Unexpected_stderr stderr -> begin
      Log.error (sprintf "Git.Unexpected_stderr %S" stderr) >>= fun () ->
      assert false
    end
  | Git.Unable_to_create_file filepath -> begin
      Log.error (sprintf "Git.Unable_to_create_file %S" filepath) >>= fun () ->
      Sys.getcwd () >>= fun path ->
      if filepath = (path ^/ ".git/index.lock") then
        Log.info (sprintf "Removing expected lockfile: %S" filepath)
        >>= fun () ->
        Sys.remove filepath
      else
        let msg = sprintf
          "Don't know what to do when Git cannot create this file: %S!" filepath
        in
        Log.error msg >>= fun () ->
        assert false
    end

let git_add_with_retry ~filepath =
  Git.add ~filepath
  >>= function
    | Ok ()   -> return ()
    | Error e -> handle_git_error e >>= fun () -> Git.add_exn ~filepath

let git_commit_with_retry ~msg =
  Git.commit ~msg
  >>= function
    | Ok ()   -> return ()
    | Error e -> handle_git_error e >>= fun () -> Git.commit_exn ~msg

let create
    ?(commits_before_gc_minor=100)
    ?(commits_before_gc_major=500)
    ~path
    ~updates_channel
    ()
  =
  Ash.mkdir ~p:() path >>= fun () ->
  Sys.chdir path       >>= fun () ->
  Git.init ()          >>= fun () ->
  Sys.getcwd ()        >>= fun path ->  (* Remember the absolute path *)
  let gitignore = ".gitignore" in
  Writer.save gitignore ~contents:"log/\n" >>= fun () ->
  let filepath = gitignore in
  git_add_with_retry ~filepath >>= fun () ->
  Git.status         ~filepath
  >>= fun statuses ->
    begin match List.map statuses ~f:simple_git_status with
    | []          -> return ()
    | [Unchanged] -> return ()
    | [Added]     -> git_commit_with_retry ~msg:(sprintf "Add %s"    filepath)
    | [Modified]  -> git_commit_with_retry ~msg:(sprintf "Update %s" filepath)
    | _           -> assert false
    end
  >>| fun () ->
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

let put_directory t filepath =
  let update_committed () =
    Pipe.write_without_pushback t.updates_channel `Committed
  in
  Sys.chdir t.path                       >>= fun () ->
  git_add_with_retry ~filepath           >>= fun () ->
  Git.status ~filepath                   >>= fun statuses ->
  let statuses =
    list_group_by (List.map statuses ~f:simple_git_status) ~f:(
      function
      | Added        -> `A
      | Modified     -> `M
      (* TODO: Decide how to handle these: *)
      | Unexpected _ -> assert false
      | Unchanged    -> assert false
    )
  in
  match statuses with
  | [] -> begin
      Log.info (sprintf "Skip: %s. 0 added/modified files." filepath)
    end
  | statuses -> begin
      Deferred.List.iter statuses ~how:`Sequential ~f:(
        let len = List.length in
        function
        | `A, statuses-> Log.info (sprintf    "Added %d files." (len statuses))
        | `M, statuses-> Log.info (sprintf "Modified %d files." (len statuses))
      )
      >>= fun () ->
      Log.info (sprintf "Commit BEGIN: %S." filepath)             >>= fun () ->
      git_commit_with_retry ~msg:(sprintf "'Update %s'" filepath) >>= fun () ->
      return
        ( incr t.commits_since_last_gc_minor
        ; incr t.commits_since_last_gc_major
        )                                                         >>= fun () ->
      Log.info (sprintf "Commit End: %S." filepath)               >>| fun () ->
      List.iter statuses ~f:(fun _ -> update_committed ())
    end

let put_object t obj_info =
  let path_to_data = Snaps_object_info.path_to_data obj_info in
  Sys.chdir t.path >>= fun () ->
  maybe_gc t       >>= fun () ->
  let p = path_to_data in
  git_add_with_retry ~filepath:p >>= fun () ->
  Git.status ~filepath:p
  >>= fun statuses ->
    begin match List.map statuses ~f:simple_git_status with
    | [Unexpected _] -> begin
        Log.info (sprintf "Skip: %S. Unknown status." p) >>| fun () ->
        Pipe.write_without_pushback t.updates_channel `Skipped
      end
    | [] -> begin
        Log.info (sprintf "Skip: %S. Known status: Unchanged" p) >>| fun () ->
        Pipe.write_without_pushback t.updates_channel `Skipped
      end
    | [Added] -> begin
        Log.info (sprintf "Commit BEGIN: %S. Known status: Added" p) >>= fun () ->
        let msg = sprintf "'Add %s'" p in
        git_commit_with_retry ~msg >>= fun () ->
        Log.info (sprintf "Commit END: %S. Known status: Added" p) >>| fun () ->
        incr t.commits_since_last_gc_minor;
        incr t.commits_since_last_gc_major;
        Pipe.write_without_pushback t.updates_channel `Committed
      end
    | [Modified] -> begin
        Log.info (sprintf "Commit BEGIN: %S. Known status: Modified" p)
        >>= fun () ->
        let msg = sprintf "'Update %s'" p in
        git_commit_with_retry ~msg >>= fun () ->
        Log.info (sprintf "Commit END: %S. Known status: Modified" p)
        >>| fun () ->
        incr t.commits_since_last_gc_minor;
        incr t.commits_since_last_gc_major;
        Pipe.write_without_pushback t.updates_channel `Committed
      end
    | _ ->
        (* TODO: Consider returning Result.t instead of assertion. *)
        assert false
    end
