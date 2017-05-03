
(* This file was auto-generated based on "src/handmade.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 1 ->
        "Expected to find a declaration here after 'let'\n"
    | 2 ->
        "Expected to find argument list\n"
    | 0 ->
        "Hint: Your program should start with a function declaration\n"
    | _ ->
        raise Not_found
