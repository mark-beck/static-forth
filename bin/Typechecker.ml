open Types
open ResultUtils

exception TypecheckException of string

module SMap = Map.Make (String)

module Typestate = struct
  type t = { stack : Ftype.t list; dict : State.dictionary_value SMap.t }

  let print_stack stack =
    print_string "TypeStack: [";
    stack |> List.map Ftype.to_string |> String.concat " " |> print_string;
    print_endline "]"
end

let resolve_poly_types typestack types_in =
  let rec resolve_poly_types' typestack types_in table =
    match (typestack, types_in) with
    | _, [] -> Ok table
    | [], _ -> Error "Type stack empty while resolving polymorphic types"
    | type_on_stack :: typestack_tl, type_in :: types_in_tl -> (
        match type_in with
        | Ftype.TGen name -> (
            match SMap.find_opt name table with
            | None ->
                resolve_poly_types' typestack_tl types_in_tl
                  (SMap.add name type_on_stack table)
            | Some resolved_type ->
                if resolved_type = type_on_stack then
                  resolve_poly_types' typestack_tl types_in_tl table
                else Error "Type mismatch while resolving polymorphic types")
        | other_t ->
            if Ftype.eq other_t type_on_stack then
              resolve_poly_types' typestack_tl types_in_tl table
            else Error "Type mismatch while resolving polymorphic types")
  in

  resolve_poly_types' typestack types_in SMap.empty

let resolve_types_out types_out table =
  let+ resolved_out =
    types_out
    |> List.map (Ftype.subst table)
    |> List.fold_left
         (fun acc e ->
           let* acc = acc in
           match e with
           | Ok e -> Ok (e :: acc)
           | Error _ ->
               Error "Unknown polymorphic type while resolving out_types")
         (Ok [])
  in
  resolved_out

(**  apply a word to the typestack by first substracting the in_types and then adding the out_types
    polymorphic types have to be resolved first **)
let apply_word (typestack : Ftype.t list) (word : State.dictionary_value) =
  let* resolving_table = resolve_poly_types typestack word.in_types in
  let typestack =
    typestack |> List.to_seq
    |> Seq.drop (List.length word.in_types)
    |> List.of_seq
  in
  let* resolved_out = resolve_types_out word.out_types resolving_table in
  Ok (List.append typestack resolved_out)

let rec type_node (node : Parser.astnode) (state : Typestate.t) :
    (Typestate.t, string) Result.t =
  let open Parser in
  match node with
  | Worddef def -> type_worddef state.dict def
  | Wordcall word -> (
      try
        let word = SMap.find word state.dict in
        let* stack = apply_word state.stack word in
        Ok { state with stack }
      with Not_found -> Error "called undefined word")
  | If nodes -> (
      match state.stack with
      | TBool :: xs ->
          let* thenstate = type_nodes nodes { state with stack = xs } in
          if List.equal ( = ) xs thenstate.stack then Ok thenstate
          else Error "if branch cant have stack effect"
      | _ -> Error "if needs a Boolean on the stack")
  | Ifelse (thennodes, elsenodes) -> (
      match state.stack with
      | TBool :: xs ->
          let* thenstate = type_nodes thennodes { state with stack = xs } in
          let* elsestate = type_nodes elsenodes { state with stack = xs } in
          if List.equal ( = ) thenstate.stack elsestate.stack then Ok thenstate
          else Error "if and else branches need to have the same stack effect"
      | _ -> Error "if needs a Boolean on the stack")
  | While nodes ->
      let* thenstate = type_nodes nodes state in
      if List.hd thenstate.stack = TBool then Ok thenstate
      else Error "while branch has to end with a Boolean"
  | String _ -> Ok { state with stack = Ftype.TString :: state.stack }
  | Number _ -> Ok { state with stack = Ftype.TNumber :: state.stack }

and type_nodes nodes state : (Typestate.t, string) result =
  match nodes with
  | [] -> Ok state
  | node :: nodes -> (
      let typestack = type_node node state in
      match typestack with
      | Error e -> Error e
      | Ok state -> type_nodes nodes state)

and type_worddef _dict _def = raise @@ Failure "not implemented"

let typecheck nodes =
  let dict = State.get_standard_dict () in
  let state = Typestate.{ stack = []; dict } in
  let* state = type_nodes nodes state in
  if List.length state.stack = 0 then Ok ()
  else Error "Stack not empty after typechecking"

let typecheck_part nodes state =
  let* state = type_nodes nodes state in
  Ok state
