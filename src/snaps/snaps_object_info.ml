open Core.Std
open Async.Std

type t =
  { path_to_data : string
  ; path_to_bucket : string
  }

let of_riak_obj ro =
  let ro_id  = ro.Riak.Object.id in
  let bucket = ro_id.Riak.Object.ID.bucket in
  let key    = ro_id.Riak.Object.ID.key in
  let (/) = Filename.concat in
  { path_to_data = "objects" / bucket / key
  ; path_to_bucket = "objects" / bucket
  }

let path_to_data {path_to_data} =
  path_to_data

let path_to_bucket {path_to_bucket} =
  path_to_bucket
