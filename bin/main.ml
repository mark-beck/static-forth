type running_error = 
  | LexerError of string * Lexer.program_text
  | ParserError of string
  | TypecheckerError of Typechecker.TypeError.t
  | InterpreterError of string

let print_error (msg : string) (text : Lexer.program_text) =
  Printf.printf "Line: %d Position: %d Error: %s\n\n" text.current_line
    text.current_position msg;
  Printf.printf "   %s\n" (List.nth text.lines text.current_line);
  Printf.printf "    %s^\n" (String.make text.current_position ' ')



let read_file filename =
  let chan = open_in filename in
  In_channel.input_all chan

let run_repl mode typestate state = 
  print_endline @@ Resources.name ^ " " ^ Resources.version ^ " REPL";
  print_endline "Type Ctrl-C to exit";
  let rec loop state typestate =
    print_string "\n> ";
    let line = read_line () in
    try
      if line = "clear\n" then loop State.{ state with stack = [] } Typechecker.Typestate.{ typestate with stack = []} else
      let tokens = Lexer.lex line in
      if mode.ArgParser.verbose then Lexer.print_tokens tokens;
      let nodes = Parser.parse tokens in
      if mode.ArgParser.verbose then Parser.print_nodes nodes;
      let typecheck_result = Typechecker.typecheck_part nodes typestate in
      match typecheck_result with
      | Ok typestate ->
        if mode.ArgParser.verbose then
          Typechecker.Typestate.print_stack
            typestate.Typechecker.Typestate.stack;
          let state = Interpreter.run_statement nodes 0 state in
          State.print_stack state.stack;
          loop state typestate
      | Error msg ->
          print_endline @@ Typechecker.TypeError.to_string msg;
          let state = Interpreter.run_statement nodes 0 state in
          State.print_stack state.stack;
          loop state typestate
    with Lexer.LexerError (msg, text) ->
      print_error msg text;
      loop state typestate
  in
  loop state typestate

let run_file typestate state filedata =
  try 
  let tokens = Lexer.lex filedata in
  Lexer.print_tokens tokens;
  let nodes = Parser.parse tokens in
  Parser.print_nodes nodes;
  let typecheck_result = Typechecker.typecheck_part nodes typestate in
  match typecheck_result with
  | Ok typestate ->
    let state = Interpreter.run_statement nodes 0 state in
    Ok (typestate, state)
  | Error msg ->
    Error (TypecheckerError msg)
  with 
  | Lexer.LexerError (msg, text) -> Error (LexerError (msg, text))
  | Parser.ParseError msg -> Error (ParserError msg)
  | Interpreter.InterpretError msg -> Error (InterpreterError msg)

let rec run_files mode typestate state files =
  match files with
  | [] -> Ok (state, typestate)
  | file :: files ->
    let filedata = read_file file in
    if mode.ArgParser.verbose then Printf.printf "Running file %s\n" file;
    match run_file typestate state filedata with
    | Ok (typestate, state) -> 
      if mode.ArgParser.verbose then Printf.printf "File %s ran successfully with\n TypeStack = %s\n ValueStack = %s\n" file (Typechecker.Typestate.show_stack typestate.Typechecker.Typestate.stack) (State.show_stack state.State.stack);
      let new_typestate = Typechecker.Typestate.{ typestate with stack = [] } in
      let new_state = State.{ state with stack = [] } in
      run_files mode new_typestate new_state files
    | Error err -> Error err




let () =
  let mode = ArgParser.parse () in
  let typestate = Typechecker.Typestate.{ stack = []; dict = State.get_standard_dict () } in
  let state = { State.stack = []; dictionary = State.get_standard_dict () } in
  let state = mode.files |> run_files mode typestate state in
  match state with
  | Ok (state, typestate) -> 
    if mode.ArgParser.repl then
    run_repl mode typestate state;
  | Error (LexerError (msg, text)) -> 
    print_string "Lexer Error: ";
    print_error msg text;
  | Error (ParserError msg) ->
    print_string "Parser Error: ";
    print_endline msg;
  | Error (TypecheckerError msg) ->
    print_string "Typechecker Error: ";
    print_endline @@ Typechecker.TypeError.to_string msg;
  | Error (InterpreterError msg) ->
    print_string "Interpreter Error: ";
    print_endline msg;
