
(* This file was auto-generated based on "lib/error_message.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 86 ->
        "Ill-formed Binding.\n"
    | 85 ->
        "Ill-formed binding.\n"
    | 83 ->
        "Ill-formed declaration.\n"
    | 82 ->
        "Ill-formed declaration.\n"
    | 80 ->
        "Ill-formed declaration.\n"
    | 76 ->
        "Ill-formed export.\n"
    | 73 ->
        "Ill-formed expression.\n"
    | 72 ->
        "Ill-formed type annotation.\n"
    | 71 ->
        "Ill-formed type annotation.\n"
    | 67 ->
        "Ill-formed expression in binding context.\n"
    | 66 ->
        "Ill-formed expression in binding.\n"
    | 65 ->
        "Ill-formed expression in binding.\n"
    | 64 ->
        "Ill-formed expression in branch.\n"
    | 61 ->
        "Ill-formed expression.\n"
    | 60 ->
        "Ill-formed expression.\n"
    | 56 ->
        "Ill-formed expression.\n"
    | 55 ->
        "Ill-formed expression.\n"
    | 53 ->
        "Ill-formed expression.\n"
    | 52 ->
        "Ill-formed expression.\n"
    | 47 ->
        "Ill-formed expression.\n"
    | 46 ->
        "Ill-formed expression.\n"
    | 45 ->
        "Ill-formed expression.\n"
    | 42 ->
        "Ill-formed expression.\n"
    | 41 ->
        "Ill-formed expression.\n"
    | 40 ->
        "Ill-formed expression.\n"
    | 37 ->
        "Ill-formed expression.\n"
    | 36 ->
        "Ill-formed expression.\n"
    | 35 ->
        "Ill-formed expression.\n"
    | 34 ->
        "Ill-formed expression.\n"
    | 31 ->
        "Ill-formed expression.\n"
    | 30 ->
        "Ill-formed expression.\n"
    | 26 ->
        "Ill-formed expression.\n"
    | 24 ->
        "Ill-formed expression.\n"
    | 23 ->
        "Ill-formed expression.\n"
    | 18 ->
        "Ill-formed expression.\n"
    | 17 ->
        "Ill-formed expression.\n"
    | 16 ->
        "Ill-formed expression.\n"
    | 15 ->
        "Ill-formed expression.\n"
    | 14 ->
        "Ill-formed expression.\n"
    | 13 ->
        "Ill-formed expression.\n"
    | 10 ->
        "Ill-formed expression.\n"
    | 9 ->
        "Ill-formed expression.\n"
    | 4 ->
        "Ill-formed expression.\n"
    | 2 ->
        "Ill-formed expression.\nNote: Use (<param1: type1, param2: type2, ...>) -> <expr> to introduce a lambda.\n"
    | 1 ->
        "Ill-formed expression.\nNote: Use \"<symbol-name>\" := <defined-symbol> to export a symbol.\n"
    | 0 ->
        "Ill-formed expression.\n"
    | _ ->
        raise Not_found
