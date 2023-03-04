
exception TypecheckException of string

module SMap = Map.Make (String)

let apply_word (typestack : FTypes.ftype list) (word : Parser.worddef) =
  word.types_in |> List.fold_left (fun typestack type_in ->
    match typestack with
    | [] -> failwith "Type stack empty"
    | type_in'::typestack' ->
      if type_in <> type_in' then failwith "Type mismatch"
      else typestack'
  ) typestack |> List.append word.types_out

let rec type_node dict node typestack =
  let open Parser in
  match node with
  | Worddef def -> type_worddef dict def
  | Wordcall word ->
    begin try
      let word = SMap.find word dict in
      apply_word typestack word
    with Not_found -> raise @@ TypecheckException "called undefined word"
  end
  | If nodes ->
    begin match typestack with
    | TBool :: xs -> 
      let thentypes = type_nodes dict nodes xs in
      if List.equal (=) xs thentypes then
        thentypes
      else
        raise @@ TypecheckException "if branch cant have stack effect"
    | _ -> raise @@ TypecheckException "if needs a Boolean on the stack"
    end
  | Ifelse (thennodes, elsenodes) ->
    begin match typestack with
    | TBool :: xs ->
      let thentypes = type_nodes dict thennodes xs in
      let elsetypes = type_nodes dict elsenodes xs in
      if List.equal (=) thentypes elsetypes then 
        thentypes
      else raise @@ TypecheckException "if and else branches need to have the same stack effect"
    | _ -> raise @@ TypecheckException "if needs a Boolean on the stack"
    end
  | While nodes ->
    let thentypes = type_nodes dict nodes typestack in
      if List.hd thentypes = TBool then
        thentypes
      else
        raise @@ TypecheckException "while branch has to end with a Boolean"
  | String _ -> TString :: typestack
  | Number _ -> TNumber :: typestack


and type_nodes dict nodes typestack =
  match nodes with
  [] -> typestack
  | node::nodes ->
    let typestack = type_node dict node typestack in
    type_nodes dict nodes typestack


and type_worddef dict def = raise @@ Failure "not implemented"
let check_word dict word = raise @@ Failure "not implemented"


