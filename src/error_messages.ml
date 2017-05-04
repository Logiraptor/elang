
(* This file was auto-generated based on "src/handmade.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 10 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 11 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 14 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 3 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 4 ->
        "Expected an argument name following this type\n"
    | 25 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 26 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 1 ->
        "Expected to find a declaration here after 'let'\n"
    | 2 ->
        "Expected to find argument list\n"
    | 0 ->
        "Hint: Your program should start with a function declaration\n"
    | _ ->
        raise Not_found
