module TokenPrinter = struct
  let token_to_string token =
    match token with
    | Lexer.Number n -> Printf.sprintf "Number: %d" n
    | Lexer.String s -> Printf.sprintf "\"%s\"" s
    | Lexer.Word w -> Printf.sprintf "%s" w
    | Lexer.IF -> Printf.sprintf "IF"
    | Lexer.ENDIF -> Printf.sprintf "ENDIF"
    | Lexer.WHILE -> Printf.sprintf "WHILE"
    | Lexer.ENDWHILE -> Printf.sprintf "ENDWHILE"
    | Lexer.Colon -> Printf.sprintf ":"
    | Lexer.Semicolon -> Printf.sprintf ";"
    | Lexer.EOF -> Printf.sprintf "EOF"

  let print_tokens tokens =
    Printf.printf "[ ";
    tokens |> List.map token_to_string |> String.concat ", " |> print_string;
    Printf.printf " ]\n"
end

let print_error (msg : string) (text : Lexer.program_text) =
  Printf.printf "Line: %d Position: %d Error: %s\n\n" text.current_line
    text.current_position msg;
  Printf.printf "   %s\n" (List.nth text.lines text.current_line);
  Printf.printf "    %s^\n" (String.make text.current_position ' ')

let () =
  print_endline "static-forth 0.0.1";
  print_endline "Type Ctrl-C to exit";
  let state = { State.stack = []; dictionary = State.get_standard_dict () } in
  let rec loop state =
    print_string "\n> ";
    let line = read_line () in
    try
      let tokens = Lexer.lex line in
      (* TokenPrinter.print_tokens tokens; *)
      let nodes = Parser.parse tokens in
      (* Parser.display_nodes nodes; *)
      let _typecheck_result = Typechecker.typecheck nodes in
      let state = Interpreter.run_statement nodes 0 state in
      State.print_stack state.stack;
      loop state
    with 
    | Lexer.LexerError (msg, text) ->
      print_error msg text;
      loop state
    | Typechecker.TypecheckException msg ->
      print_string msg;
      loop state
  in
  loop state
