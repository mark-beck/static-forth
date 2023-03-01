open FTypes

type worddef = {
  name : string;
  types_in : ftype list;
  types_out : ftype list;
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

let display_nodes =
  let rec display_node (node : astnode) =
    match node with
    | Worddef worddef ->
        Printf.printf "Worddef %s [" worddef.name;
        List.iter
          (fun token ->
            display_node token;
            Printf.printf " ")
          worddef.nodes;
        Printf.printf "]"
    | Wordcall word -> Printf.printf "Wordcall %s" word
    | String str -> Printf.printf "String %s" str
    | Number num -> Printf.printf "Number %d" num
    | If nodes ->
        Printf.printf "If [";
        List.iter
          (fun node ->
            display_node node;
            Printf.printf " ")
          nodes;
        Printf.printf "]"
    | Ifelse (nodes1, nodes2) ->
        Printf.printf "Ifelse [";
        List.iter
          (fun node ->
            display_node node;
            Printf.printf " ")
          nodes1;
        Printf.printf "] [";
        List.iter
          (fun node ->
            display_node node;
            Printf.printf " ")
          nodes2;
        Printf.printf "]"
    | While nodes ->
        Printf.printf "While [";
        List.iter
          (fun node ->
            display_node node;
            Printf.printf " ")
          nodes;
        Printf.printf "]"
  in
  List.iter (fun node ->
      display_node node;
      Printf.printf "\n")

let get pos tokens = try Option.some @@ List.nth tokens pos with _ -> None

let rec parse_worddef (tokens : Lexer.t list) pos =
  let word_beeing_defined =
    match get pos tokens with
    | Some (Lexer.Word word) -> word
    | _ -> raise @@ Failure "Expected identifier after ':'"
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
    | Some Lexer.EOF -> raise @@ Failure "Unexpected EOF"
    | Some _ -> raise @@ Failure "Unexpected token"
    | None -> raise @@ Failure "OUT OF TOKENS"
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
    | Some Lexer.EOF -> raise @@ Failure "Unexpected EOF"
    | Some _ -> raise @@ Failure "Unexpected token"
    | None -> raise @@ Failure "OUT OF TOKENS"
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
    | Some Lexer.EOF -> raise @@ Failure "Unexpected EOF"
    | Some _ -> raise @@ Failure "Unexpected token"
    | None -> raise @@ Failure "OUT OF TOKENS"
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
  | Some Lexer.Colon ->
      let pos, node = parse_worddef tokens (pos + 1) in
      parse_statement tokens pos (node :: nodes)
  | Some Lexer.EOF -> List.rev nodes
  | Some _ -> raise @@ Failure "Unexpected token"
  | None -> raise @@ Failure "OUT OF TOKENS"

and parse tokens = parse_statement tokens 0 []
