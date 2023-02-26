

module TokenPrinter = struct
  let token_to_string token =
    match token with
    | Parser.Number n -> Printf.sprintf "Number: %d" n
    | Parser.String s -> Printf.sprintf "\"%s\"" s
    | Parser.Word w -> Printf.sprintf "%s" w
    | Parser.IF -> Printf.sprintf "IF"
    | Parser.ENDIF -> Printf.sprintf "ENDIF"
    | Parser.WHILE -> Printf.sprintf "WHILE"
    | Parser.ENDWHILE -> Printf.sprintf "ENDWHILE"
    | Parser.Colon -> Printf.sprintf ":"
    | Parser.Semicolon -> Printf.sprintf ";"
    | Parser.EOF -> Printf.sprintf "EOF"

  let print_tokens tokens =
    Printf.printf "[ ";
    tokens |> List.map token_to_string |> String.concat ", " |> print_string;
    Printf.printf " ]\n"
end


let print_error (msg : string) (text : Parser.program_text) =
  Printf.printf "Line: %d Position: %d Error: %s\n\n" text.current_line text.current_position msg;
  Printf.printf "   %s\n" (List.nth text.lines text.current_line);
  Printf.printf "    %s^\n" (String.make text.current_position ' ')



let () = 
  print_endline "static-forth 0.0.1";
  print_endline "Type Ctrl-C to exit";
  let state = { Interpreter.stack = []; dictionary = Interpreter.get_standard_dict (); Interpreter.scope_end = [] } in
  let rec loop state =
    print_string "> ";
    let line = read_line () in
    try
      let tokens = Parser.parse line in
      TokenPrinter.print_tokens tokens;
      loop @@ Interpreter.run_statement tokens 0 state;
      
    with
    | Parser.LexerError (msg, text) -> print_error msg text;
      loop state in
  loop state





