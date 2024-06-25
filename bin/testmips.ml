let () =
  let filename = Sys.argv.(1) in
  let f = In_channel.open_text filename in
  let b = Lexing.from_channel f in
  Lexing.set_filename b filename;
  Mmips.Compiler.mmips b |> print_endline
;;
