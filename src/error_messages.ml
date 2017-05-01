
(* This file was auto-generated based on "src/handmade.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 1 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 1>\n"
    | 2 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 2>\n"
    | 3 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 3>\n"
    | 9 ->
        "Unexpected token after function declaration\n"
    | 0 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 0>\n"
    | _ ->
        raise Not_found
