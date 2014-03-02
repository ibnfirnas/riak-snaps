open Core.Std
open Async.Std

let init () =
  Log.Global.set_level `Info;
  Log.Global.set_output [Log.Output.stderr ()];
  return ()

let info msg =
  Log.Global.info "%s" msg;
  Log.Global.flushed ()
