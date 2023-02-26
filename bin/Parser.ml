type t =
  | Number of int
  | String of string
  | Word of string
  | IF
  | ENDIF
  | WHILE
  | ENDWHILE
  | Colon
  | Semicolon
  | EOF

type program_text = {
  lines : string list;
  current_line : int;
  current_position : int;
}

exception LexerError of string * program_text

let current program_text =
  try
    let line = List.nth program_text.lines program_text.current_line in
    Option.some @@ String.get line program_text.current_position
  with _ -> None

let peek n program_text =
  try
    let line = List.nth program_text.lines program_text.current_line in
    Option.some @@ String.get line (program_text.current_position + n)
  with _ -> None

let skip n program_text =
  let line = List.nth program_text.lines program_text.current_line in
  let new_position = program_text.current_position + n in
  if new_position >= String.length line then
    {
      lines = program_text.lines;
      current_line = program_text.current_line + 1;
      current_position = 0;
    }
  else
    {
      lines = program_text.lines;
      current_line = program_text.current_line;
      current_position = new_position;
    }

let next program_text = skip 1 program_text

let check_is_word word text =
  let rec loop text i =
    if i >= String.length word then true
    else
      match current text with
      | Some c when Char.lowercase_ascii c = Char.lowercase_ascii word.[i] ->
          loop (next text) (i + 1)
      | _ -> false
  in
  loop text 0

let rec lex text res =
  match current text with
  | Some ' ' -> lex (next text) res
  | Some ':' -> lex (next text) (Colon :: res)
  | Some ';' -> lex (next text) (Semicolon :: res)
  | Some '0' .. '9' ->
      let num, text = consume_number text [] in
      lex text (Number num :: res)
  | Some '"' ->
      let str, xs = consume_string (next text) [] in
      lex xs (String str :: res)
  | Some _ when check_is_word "if" text -> lex (skip 2 text) (IF :: res)
  | Some _ when check_is_word "endif" text -> lex (skip 5 text) (ENDIF :: res)
  | Some _ when check_is_word "while" text -> lex (skip 5 text) (WHILE :: res)
  | Some _ when check_is_word "endwhile" text ->
      lex (skip 8 text) (ENDWHILE :: res)
  | Some _ ->
      let word, xs = consume_word text [] in
      lex xs (Word word :: res)
  | None -> List.rev (EOF :: res)

and consume_number text res =
  match current text with
  | Some ('0' .. '9' as c) -> consume_number (next text) (c :: res)
  | Some ' ' | Some ';' | None ->
      ( res |> List.rev
        |> List.map (String.make 1)
        |> String.concat "" |> int_of_string,
        text )
  | Some _ ->
      raise @@ LexerError ("Unexpected character while parsing number", text)

and consume_string text res =
  match current text with
  | Some '"' ->
      if peek 1 text = Some ' ' || peek 1 text = Some ';' || peek 1 text = None
      then
        (* after string you can only have whitespace, ; or EOF *)
        ( res |> List.rev |> List.map (String.make 1) |> String.concat "",
          next text )
      else raise @@ LexerError ("Unexpected character after string", text)
  | Some (_ as x) -> consume_string (next text) (x :: res)
  | None ->
      raise @@ LexerError ("Unexpected end of file while parsing string", text)

and consume_word text res =
  match current text with
  | Some ' ' | Some ';' | None ->
      (res |> List.rev |> List.map (String.make 1) |> String.concat "", text)
  | Some (_ as x) -> consume_word (next text) (x :: res)

let parse str =
  lex { lines = [ str ]; current_line = 0; current_position = 0 } []
