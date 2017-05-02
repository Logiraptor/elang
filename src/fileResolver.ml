open Core.Std

type filetype = {
  chan: In_channel.t;
  filename: string
}

let resolve name =
  Ok ({chan=In_channel.create name; filename=name})
