open Types
open ResultUtils

module TypeError = struct
  type t =
    | Missing of string * Ftype.t list
    | TypeMismatch of string * Ftype.t * Ftype.t
    | UnknownWord of string * string
    | UnknownType of string * Ftype.t
    | Other of string

  let to_string = function
    | Missing (s, ts) ->
        s ^ ": " ^ (ts |> List.map Ftype.to_string |> String.concat ", ")
    | TypeMismatch (s, t1, t2) ->
        s ^ ": " ^ Ftype.to_string t1 ^ " and " ^ Ftype.to_string t2
    | UnknownWord (s, w) -> s ^ ": " ^ w
    | UnknownType (s, t) -> s ^ ": " ^ Ftype.to_string t
    | Other s -> s
end

module TE = TypeError
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
    | [], t :: _ ->
        Error
          (TE.Missing
             ("Type stack empty while resolving polymorphic types", [ t ]))
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
                else
                  Error
                    (TypeMismatch
                       ( "Type mismatch while resolving polymorphic types",
                         resolved_type,
                         type_on_stack )))
        | other_t ->
            if Ftype.eq other_t type_on_stack then
              resolve_poly_types' typestack_tl types_in_tl table
            else
              Error
                (TypeMismatch
                   ( "Type mismatch while resolving polymorphic types",
                     other_t,
                     type_on_stack )))
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
           | Error t ->
               Error
                 (TE.UnknownType
                    ("Unknown polymorphic type while resolving out_types", t)))
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
    (Typestate.t, TypeError.t) Result.t =
  let open Parser in
  match node with
  | Worddef def ->
      let* typed_def = type_worddef state.dict def in
      let dict = SMap.add typed_def.name typed_def state.dict in
      Ok { state with dict }
  | Wordcall word -> (
      try
        let word' = SMap.find word state.dict in
        let* stack = apply_word state.stack word' in
        Ok { state with stack }
      with Not_found -> Error (UnknownWord ("called undefined word", word)))
  | If nodes -> (
      match state.stack with
      | TBool :: xs ->
          let* thenstate = type_nodes nodes { state with stack = xs } in
          if List.equal ( = ) xs thenstate.stack then Ok thenstate
          else Error (Other "if branch cant have stack effect")
      | _ ->
          Error (Missing ("if needs a Boolean on the stack", [ Ftype.TBool ])))
  | Ifelse (thennodes, elsenodes) -> (
      match state.stack with
      | TBool :: xs ->
          let* thenstate = type_nodes thennodes { state with stack = xs } in
          let* elsestate = type_nodes elsenodes { state with stack = xs } in
          if List.equal ( = ) thenstate.stack elsestate.stack then Ok thenstate
          else
            Error
              (Other "if and else branches need to have the same stack effect")
      | _ ->
          Error (Missing ("if needs a Boolean on the stack", [ Ftype.TBool ])))
  | While nodes ->
      let* thenstate = type_nodes nodes state in
      if List.hd thenstate.stack = TBool then Ok thenstate
      else
        Error
          (Missing ("while branch has to end with a Boolean", [ Ftype.TBool ]))
  | String _ -> Ok { state with stack = Ftype.TString :: state.stack }
  | Number _ -> Ok { state with stack = Ftype.TNumber :: state.stack }

and type_nodes nodes state : (Typestate.t, TypeError.t) result =
  match nodes with
  | [] -> Ok state
  | node :: nodes ->
      let* state = type_node node state in
      type_nodes nodes state

(**  typecheck a worddef by trying to typecheck its nodes and then iteratively adding the missing types to the input types until it succeeds
    the remaining typestack is the outtput types **)
and type_worddef :
    State.dictionary_value SMap.t ->
    Parser.worddef ->
    (State.dictionary_value, TypeError.t) result =
 fun dict def ->
  let rec type_worddef' dict nodes in_types =
    let state = Typestate.{ stack = in_types; dict } in
    let nodes_result = type_nodes nodes state in
    match nodes_result with
    | Ok state ->
        Ok
          State.
            {
              name = def.name;
              in_types;
              out_types = state.stack;
              impl = User def.nodes;
            }
    | Error (Missing (_, ts)) -> type_worddef' dict nodes (ts @ in_types)
    | Error e -> Error e
  in
  type_worddef' dict def.nodes []

let typecheck nodes =
  let dict = State.get_standard_dict () in
  let state = Typestate.{ stack = []; dict } in
  let* state = type_nodes nodes state in
  if List.length state.stack = 0 then Ok ()
  else Error (Missing ("Stack not empty after typechecking", state.stack))

let typecheck_part nodes state =
  let* state = type_nodes nodes state in
  Ok state
