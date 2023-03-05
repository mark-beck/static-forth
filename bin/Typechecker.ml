open Types

exception TypecheckException of string

module SMap = Map.Make (String)

let resolve_poly_types typestack types_in =
  let rec resolve_poly_types' typestack types_in table =
    match (typestack, types_in) with
    | _, [] -> table
    | [], _ -> raise @@ TypecheckException "Type stack empty while resolving polymorphic types"
    | type_on_stack :: typestack_tl, type_in :: types_in_tl ->
      match type_in with
      | Ftype.TGen name ->
        begin match SMap.find_opt name table with
        | None -> resolve_poly_types' typestack_tl types_in_tl (SMap.add name type_on_stack table)
        | Some resolved_type ->
          if resolved_type = type_on_stack then
            resolve_poly_types' typestack_tl types_in_tl table
          else
            raise @@ TypecheckException "Type mismatch while resolving polymorphic types"
        end
      | other_t -> if Ftype.eq other_t type_on_stack then
          resolve_poly_types' typestack_tl types_in_tl table
        else
          raise @@ TypecheckException "Type mismatch while resolving polymorphic types"
        
  in
  resolve_poly_types' typestack types_in SMap.empty

let resolve_types_out typestack types_out table =
  let resolved_out = List.map (fun t ->
    match Ftype.subst table t with 
    | Ok ty -> ty 
    | Error _ -> raise @@ TypecheckException "cant find polymorphic types in dict"
    ) 
    types_out in
  resolved_out @ typestack

(**  apply a word to the typestack by first substracting the in_types and then adding the out_types
    polymorphic types have to be resolved first **)
let apply_word (typestack : Ftype.t list) (word : State.dictionary_value) =
  let resolving_table = resolve_poly_types typestack word.in_types in
  let resolved_out = resolve_types_out typestack word.out_types resolving_table in
  resolved_out

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


and type_worddef _dict _def = raise @@ Failure "not implemented"

let typecheck nodes =
  let dict = State.get_standard_dict () in
  let typestack = type_nodes dict nodes [] in
  if List.length typestack = 0 then
    Ok ()
  else
    Error "Stack not empty after typechecking"


