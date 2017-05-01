open Core.Std

let resolve name : In_channel.t =
  In_channel.create name
