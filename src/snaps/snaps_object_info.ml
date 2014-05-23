open Core.Std
open Async.Std

type t =
  { data_path   : string
  ; bucket_path : string
  }

let of_riak_obj ro =
  let ro_id  = ro.Riak.Object.id in
  let bucket = ro_id.Riak.Object.ID.bucket in
  let key    = ro_id.Riak.Object.ID.key in
  let (/) = Filename.concat in
  { data_path   = "objects" / bucket / key
  ; bucket_path = "objects" / bucket
  }

let to_data_path {data_path} =
  data_path

let to_bucket_path {bucket_path} =
  bucket_path
