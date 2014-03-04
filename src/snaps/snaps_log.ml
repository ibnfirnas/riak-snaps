open Core.Std
open Async.Std

module type CALLER = sig
  val name : string
end

module Make (Caller : CALLER) = struct
  let init () =
    Log.Global.set_level `Info;
    Log.Global.set_output [Log.Output.stderr ()];
    return ()

  let info msg =
    Log.Global.info "| %s | %s" Caller.name msg;
    Log.Global.flushed ()

  let error msg =
    Log.Global.error "| %s | %s" Caller.name msg;
    Log.Global.flushed ()
end
