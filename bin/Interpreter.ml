module SMap = Map.Make (String)

type stack_value = Number of int | String of string | Bool of bool

and dictionary_value =
  | Builtin of (state -> state)
  | UserDefined of Parser.t list
  | Link of string

and state = {
  stack : stack_value list;
  dictionary : dictionary_value SMap.t;
  scope_end : int list;
}

let get_standard_dict () =
  let dict = SMap.empty in
  let dict =
    SMap.add "T"
      (Builtin (fun state -> { state with stack = Bool true :: state.stack }))
      dict
  in
  let dict =
    SMap.add "F"
      (Builtin (fun state -> { state with stack = Bool false :: state.stack }))
      dict
  in
  let dict =
    SMap.add "add"
      (Builtin
         (fun state ->
           match state.stack with
           | Number a :: Number b :: xs ->
               { state with stack = Number (a + b) :: xs }
           | _ -> raise @@ Failure "Stack underflow"))
      dict
  in
  let dict =
    SMap.add "sub"
      (Builtin
         (fun state ->
           match state.stack with
           | Number a :: Number b :: xs ->
               { state with stack = Number (a - b) :: xs }
           | _ -> raise @@ Failure "Stack underflow"))
      dict
  in
  let dict =
    SMap.add "mul"
      (Builtin
         (fun state ->
           match state.stack with
           | Number a :: Number b :: xs ->
               { state with stack = Number (a * b) :: xs }
           | _ -> raise @@ Failure "Stack underflow"))
      dict
  in
  let dict =
    SMap.add "div"
      (Builtin
         (fun state ->
           match state.stack with
           | Number a :: Number b :: xs ->
               { state with stack = Number (a / b) :: xs }
           | _ -> raise @@ Failure "Stack underflow"))
      dict
  in
  let dict =
    SMap.add "dup"
      (Builtin
         (fun state ->
           match state.stack with
           | x :: xs -> { state with stack = x :: x :: xs }
           | _ -> raise @@ Failure "Stack underflow"))
      dict
  in
  let dict =
    SMap.add "drop"
      (Builtin
         (fun state ->
           match state.stack with
           | _ :: xs -> { state with stack = xs }
           | _ -> raise @@ Failure "Stack underflow"))
      dict
  in
  let dict =
    SMap.add "swap"
      (Builtin
         (fun state ->
           match state.stack with
           | a :: b :: xs -> { state with stack = b :: a :: xs }
           | _ -> raise @@ Failure "Stack underflow"))
      dict
  in
  let dict =
    SMap.add "rot"
      (Builtin
         (fun state ->
           match state.stack with
           | a :: b :: c :: xs -> { state with stack = c :: a :: b :: xs }
           | _ -> raise @@ Failure "Stack underflow"))
      dict
  in
  let dict =
    SMap.add "print"
      (Builtin
         (fun state ->
           match state.stack with
           | Number n :: xs ->
               print_int n;
               print_newline ();
               { state with stack = xs }
           | String s :: xs ->
               print_string s;
               print_newline ();
               { state with stack = xs }
           | Bool b :: xs ->
               print_string (if b then "true" else "false");
               print_newline ();
               { state with stack = xs }
           | _ -> raise @@ Failure "Stack underflow in print"))
      dict
  in
  let dict =
    SMap.add "print_stack"
      (Builtin
         (fun state ->
           state.stack
           |> List.iter (function
                | Number n ->
                    print_int n;
                    print_string " "
                | String s ->
                    print_string s;
                    print_string " "
                | Bool b ->
                    print_string (if b then "true" else "false");
                    print_string " ");
           print_newline ();
           state))
      dict
  in
  let dict =
    SMap.add "print_dict"
      (Builtin
         (fun state ->
           SMap.iter
             (fun k _ ->
               print_string k;
               print_string " ")
             state.dictionary;
           print_newline ();
           state))
      dict
  in
  dict

let get pos tokens = try Option.some @@ List.nth tokens pos with _ -> None

let rec run_statement tokens pos state =
  match get pos tokens with
  | Some (Parser.Number n) ->
      run_statement tokens (pos + 1)
        { state with stack = Number n :: state.stack }
  | Some (Parser.String s) ->
      run_statement tokens (pos + 1)
        { state with stack = String s :: state.stack }
  | Some (Parser.Word word) ->
      run_statement tokens (pos + 1) (run_word word state)
  | Some Parser.Colon ->
      let pos, state = new_word tokens (pos + 1) state in
      run_statement tokens pos state
  | Some Parser.Semicolon -> raise @@ Failure "Unexpected ';'"
  | Some Parser.IF ->
      let state = find_endif tokens (pos + 1) 0 state in
      run_if tokens (pos + 1) state
  | Some Parser.ENDIF -> run_statement tokens (pos + 1) (dropscope state)
  | Some EOF -> state
  | None -> raise @@ Failure "Out of tokens"
  | _ -> raise @@ Failure "Unexpected token"

and run_word word state =
  match SMap.find_opt word state.dictionary with
  | Some (Builtin f) -> f state
  | Some (UserDefined tokens) -> run_statement tokens 0 state
  | Some (Link link) -> run_word link state
  | None -> raise @@ Failure "Word not found"

and new_word tokens pos state =
  let word_beeing_defined =
    match get pos tokens with
    | Some (Parser.Word word) -> word
    | _ -> raise @@ Failure "Expected identifier after ':'"
  in
  let rec loop tokens pos body =
    match get pos tokens with
    | Some Parser.Semicolon ->
        ( pos + 1,
          {
            state with
            dictionary =
              SMap.add word_beeing_defined
                (UserDefined (List.rev (Parser.EOF :: body)))
                state.dictionary;
          } )
    | Some Parser.EOF -> raise @@ Failure "Unexpected EOF"
    | Some token -> loop tokens (pos + 1) (token :: body)
    | None -> raise @@ Failure "OUT OF TOKENS"
  in
  loop tokens (pos + 1) []

and find_endif tokens pos scope state =
  match get pos tokens with
  | Some Parser.IF -> find_endif tokens (pos + 1) (scope + 1) state
  | Some Parser.ENDIF when scope = 0 ->
      { state with scope_end = pos :: state.scope_end }
  | Some Parser.ENDIF -> find_endif tokens (pos + 1) (scope - 1) state
  | Some Parser.EOF -> raise @@ Failure "Unexpected EOF"
  | Some _ -> find_endif tokens (pos + 1) scope state
  | None -> raise @@ Failure "OUT OF TOKENS"

and find_endwhile tokens pos scope state =
  match get pos tokens with
  | Some Parser.WHILE -> find_endwhile tokens (pos + 1) (scope + 1) state
  | Some Parser.ENDWHILE when scope = 0 ->
      { state with scope_end = pos :: state.scope_end }
  | Some Parser.ENDWHILE -> find_endwhile tokens (pos + 1) (scope - 1) state
  | Some Parser.EOF -> raise @@ Failure "Unexpected EOF"
  | Some _ -> find_endwhile tokens (pos + 1) scope state
  | None -> raise @@ Failure "OUT OF TOKENS"

and dropscope state =
  match state.scope_end with
  | _ :: xs -> { state with scope_end = xs }
  | _ -> raise @@ Failure "Not in scope"

and get_jumppoint state =
  match state.scope_end with
  | x :: _ -> x
  | _ -> raise @@ Failure "Not in scope"

and run_if tokens pos state =
  match state.stack with
  | [] -> raise @@ Failure "Stack underflow"
  | Bool true :: xs ->
      let state = { state with stack = xs } in
      run_statement tokens pos state
  | Bool false :: xs ->
      let jumppoint = get_jumppoint state in
      let state = { state with stack = xs } in
      run_statement tokens jumppoint state
  | _ -> raise @@ Failure "Expected boolean on stack"
