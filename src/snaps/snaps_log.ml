open Core.Std
open Async.Std

module type CALLER = sig
  val name : string
end

module Make (Caller : CALLER) = struct
  let info msg =
    Log.Global.info "| %s | %s" Caller.name msg;
    Log.Global.flushed ()

  let error msg =
    Log.Global.error "| %s | %s" Caller.name msg;
    Log.Global.flushed ()
end

module Shell = Core_extended.Shell

let init ~level ~repo_path =
  let (/) = Filename.concat in
  let log_dir = repo_path / "log" in
  Shell.mkdir ~p:() log_dir;
  let filename = log_dir / "snaps.log" in
  Log.Global.set_level level;
  Log.Global.set_output [Log.Output.file `Text ~filename]
