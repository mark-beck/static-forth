module SMap = Map.Make (String)
open State
open Types.Vtype

exception InterpretError of string

let get pos nodes = try Option.some @@ List.nth nodes pos with _ -> None

let rec run_statement nodes pos state =
  match get pos nodes with
  | Some (Parser.Worddef worddef) ->
      run_statement nodes (pos + 1) (State.add_word worddef state)
  | Some (Parser.Wordcall word) ->
      run_statement nodes (pos + 1) (run_word word state)
  | Some (Parser.If ifdef) -> run_statement nodes (pos + 1) (run_if ifdef state)
  | Some (Parser.While whiledef) ->
      run_statement nodes (pos + 1) (run_while whiledef state)
  | Some (Parser.Ifelse (thenblock, elseblock)) ->
      run_statement nodes (pos + 1) (run_ifelse thenblock elseblock state)
  | Some (Parser.Number _ as n) -> run_statement nodes (pos + 1) (push n state)
  | Some (Parser.String _ as s) -> run_statement nodes (pos + 1) (push s state)
  | None -> state

and run_word word state =
  let word = SMap.find_opt word state.dictionary in
  match word with
  | Some { impl = Builtin f; _ } -> f state
  | Some { impl = User tokens; _ } -> run_statement tokens 0 state
  | None -> raise @@ InterpretError "Word not found"

and run_if block state =
  let state, condition = pop state in
  match condition with
  | VBool true -> run_statement block 0 state
  | VBool false -> state
  | _ -> raise @@ InterpretError "Not a boolean on stack for if"

and run_while block state =
  let state, condition = pop state in
  match condition with
  | VBool true -> run_while block @@ run_statement block 0 state
  | VBool false -> state
  | _ -> raise @@ InterpretError "Not a boolean on stack for while"

and run_ifelse thenblock elseblock state =
  let newstate, condition = pop state in
  match condition with
  | VBool true -> run_statement thenblock 0 newstate
  | VBool false -> run_statement elseblock 0 newstate
  | _ -> raise @@ InterpretError "Not a boolean on stack for ifelse"

and pop state =
  match state.stack with
  | [] -> raise @@ InterpretError "Stack underflow"
  | x :: xs -> ({ state with stack = xs }, x)

and push value state =
  match value with
  | Parser.Number n -> { state with stack = VNumber n :: state.stack }
  | Parser.String s -> { state with stack = VString s :: state.stack }
  | _ -> raise @@ InterpretError "Invalid value to push"

let run tokens =
  let state = { stack = []; dictionary = SMap.empty } in
  run_statement tokens 0 state
