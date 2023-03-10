open Types

exception ParseError of string

type worddef = {
  name : string;
  types_in : Ftype.t list;
  types_out : Ftype.t list;
  nodes : astnode list;
}

and astnode =
  | Worddef of worddef
  | Wordcall of string
  | String of string
  | Number of int
  | If of astnode list
  | Ifelse of (astnode list * astnode list)
  | While of astnode list
  | For of astnode list

let rec show_nodes nodes = 
  "[ " ^ (nodes |> List.map (function
  | Worddef worddef -> Printf.sprintf "Worddef %s %s" worddef.name (show_nodes worddef.nodes)
  | Wordcall word -> Printf.sprintf "Wordcall %s" word
  | String str -> Printf.sprintf "String %s" str
  | Number num -> Printf.sprintf "Number %d" num
  | If nodes -> Printf.sprintf "If %s" (show_nodes nodes)
  | Ifelse (nodes1, nodes2) -> Printf.sprintf "Ifelse %s %s" (show_nodes nodes1) (show_nodes nodes2)
  | While nodes -> Printf.sprintf "While %s" (show_nodes nodes)
  | For nodes -> Printf.sprintf "For %s" (show_nodes nodes)
  ) |> String.concat ", ") ^ " ]"

let print_nodes nodes = Printf.printf "Parsed nodes: %s\n" (show_nodes nodes)

let get pos tokens = try Option.some @@ List.nth tokens pos with _ -> None

let rec parse_worddef (tokens : Lexer.t list) pos =
  let word_beeing_defined =
    match get pos tokens with
    | Some (Lexer.Word word) -> word
    | _ -> raise @@ ParseError "Expected identifier after ':'"
  in
  let rec loop tokens pos body =
    match get pos tokens with
    | Some (Lexer.Word word) -> loop tokens (pos + 1) (Wordcall word :: body)
    | Some (Lexer.String str) -> loop tokens (pos + 1) (String str :: body)
    | Some (Lexer.Number num) -> loop tokens (pos + 1) (Number num :: body)
    | Some Lexer.IF ->
        let pos, node = parse_if tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.WHILE ->
        let pos, node = parse_while tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.FOR -> 
        let pos, node = parse_for tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.Colon ->
        let pos, node = parse_worddef tokens pos in
        loop tokens pos (node :: body)
    | Some Lexer.Semicolon ->
        ( pos + 1,
          Worddef
            {
              name = word_beeing_defined;
              types_in = [];
              types_out = [];
              nodes = List.rev body;
            } )
    | Some Lexer.EOF -> raise @@ ParseError "Unexpected EOF"
    | Some _ -> raise @@ ParseError "Unexpected token in worddef"
    | None -> raise @@ ParseError "OUT OF TOKENS"
  in
  loop tokens (pos + 1) []

and parse_if tokens pos =
  let rec loop tokens pos body =
    match get pos tokens with
    | Some (Lexer.Word word) -> loop tokens (pos + 1) (Wordcall word :: body)
    | Some (Lexer.String str) -> loop tokens (pos + 1) (String str :: body)
    | Some (Lexer.Number num) -> loop tokens (pos + 1) (Number num :: body)
    | Some Lexer.IF ->
        let pos, node = parse_if tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.WHILE ->
        let pos, node = parse_while tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.Colon ->
        let pos, node = parse_worddef tokens pos in
        loop tokens pos (node :: body)
    | Some Lexer.ENDIF -> (pos + 1, If (List.rev body))
    | Some Lexer.EOF -> raise @@ ParseError "Unexpected EOF"
    | Some _ -> raise @@ ParseError "Unexpected token in if"
    | None -> raise @@ ParseError "OUT OF TOKENS"
  in
  loop tokens pos []

and parse_while tokens pos =
  let rec loop tokens pos body =
    match get pos tokens with
    | Some (Lexer.Word word) -> loop tokens (pos + 1) (Wordcall word :: body)
    | Some (Lexer.String str) -> loop tokens (pos + 1) (String str :: body)
    | Some (Lexer.Number num) -> loop tokens (pos + 1) (Number num :: body)
    | Some Lexer.IF ->
        let pos, node = parse_if tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.WHILE ->
        let pos, node = parse_while tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.Colon ->
        let pos, node = parse_worddef tokens pos in
        loop tokens pos (node :: body)
    | Some Lexer.ENDWHILE -> (pos + 1, While (List.rev body))
    | Some Lexer.EOF -> raise @@ ParseError "Unexpected EOF"
    | Some _ -> raise @@ ParseError "Unexpected token in while"
    | None -> raise @@ ParseError "OUT OF TOKENS"
  in
  loop tokens pos []

  and parse_for tokens pos =
  let rec loop tokens pos body =
    match get pos tokens with
    | Some (Lexer.Word word) -> loop tokens (pos + 1) (Wordcall word :: body)
    | Some (Lexer.String str) -> loop tokens (pos + 1) (String str :: body)
    | Some (Lexer.Number num) -> loop tokens (pos + 1) (Number num :: body)
    | Some Lexer.IF ->
        let pos, node = parse_if tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.WHILE ->
        let pos, node = parse_while tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.FOR -> 
        let pos, node = parse_for tokens (pos + 1) in
        loop tokens pos (node :: body)
    | Some Lexer.Colon ->
        let pos, node = parse_worddef tokens pos in
        loop tokens pos (node :: body)
    | Some Lexer.ENDFOR -> (pos + 1, For (List.rev body))
    | Some Lexer.EOF -> raise @@ ParseError "Unexpected EOF"
    | Some _ -> raise @@ ParseError "Unexpected token in for"
    | None -> raise @@ ParseError "OUT OF TOKENS"
  in
  loop tokens pos []

and parse_statement tokens pos nodes =
  match get pos tokens with
  | Some (Lexer.Word word) ->
      parse_statement tokens (pos + 1) (Wordcall word :: nodes)
  | Some (Lexer.String str) ->
      parse_statement tokens (pos + 1) (String str :: nodes)
  | Some (Lexer.Number num) ->
      parse_statement tokens (pos + 1) (Number num :: nodes)
  | Some Lexer.IF ->
      let pos, node = parse_if tokens (pos + 1) in
      parse_statement tokens pos (node :: nodes)
  | Some Lexer.WHILE ->
      let pos, node = parse_while tokens (pos + 1) in
      parse_statement tokens pos (node :: nodes)
  | Some Lexer.FOR ->
      let pos, node = parse_for tokens (pos + 1) in
      parse_statement tokens pos (node :: nodes)
  | Some Lexer.Colon ->
      let pos, node = parse_worddef tokens (pos + 1) in
      parse_statement tokens pos (node :: nodes)
  | Some Lexer.EOF -> List.rev nodes
  | Some _ -> raise @@ ParseError "Unexpected token in statement"
  | None -> raise @@ ParseError "OUT OF TOKENS"

and parse tokens = parse_statement tokens 0 []
