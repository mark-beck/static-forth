open FTypes
module SMap = Map.Make (String)

type implementation =
  | Builtin of (state -> state)
  | User of Parser.astnode list

and state = { stack : stack_value list; dictionary : dictionary_value SMap.t }

and dictionary_value = {
  name : string;
  impl : implementation;
  type_in : ftype list;
  type_out : ftype list;
}

let add_word (worddef : Parser.worddef) state =
  let word =
    {
      name = worddef.name;
      impl = User worddef.nodes;
      type_in = worddef.types_in;
      type_out = worddef.types_out;
    }
  in
  { state with dictionary = SMap.add worddef.name word state.dictionary }

let get_standard_dict () =
  let words =
    [
      {
        name = "T";
        impl =
          Builtin
            (fun state -> { state with stack = VBool true :: state.stack });
        type_in = [];
        type_out = [ TBool ];
      };
      {
        name = "F";
        impl =
          Builtin
            (fun state -> { state with stack = VBool false :: state.stack });
        type_in = [];
        type_out = [ TBool ];
      };
      {
        name = "add";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | VNumber a :: VNumber b :: xs ->
                  { state with stack = VNumber (a + b) :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TNumber; TNumber ];
        type_out = [ TNumber ];
      };
      {
        name = "sub";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | VNumber a :: VNumber b :: xs ->
                  { state with stack = VNumber (a - b) :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TNumber; TNumber ];
        type_out = [ TNumber ];
      };
      {
        name = "mul";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | VNumber a :: VNumber b :: xs ->
                  { state with stack = VNumber (a * b) :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TNumber; TNumber ];
        type_out = [ TNumber ];
      };
      {
        name = "div";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | VNumber a :: VNumber b :: xs ->
                  { state with stack = VNumber (a / b) :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TNumber; TNumber ];
        type_out = [ TNumber ];
      };
      {
        name = "eq";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | VNumber a :: VNumber b :: xs ->
                  { state with stack = VBool (a = b) :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TNumber; TNumber ];
        type_out = [ TBool ];
      };
      {
        name = "not";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | VBool a :: xs -> { state with stack = VBool (not a) :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TBool ];
        type_out = [ TBool ];
      };
      {
        name = "dup";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | x :: xs -> { state with stack = x :: x :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TAny ];
        type_out = [ TAny; TAny ];
      };
      {
        name = "drop";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | _ :: xs -> { state with stack = xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TAny ];
        type_out = [];
      };
      {
        name = "swap";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: b :: xs -> { state with stack = b :: a :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TAny; TAny ];
        type_out = [ TAny; TAny ];
      };
      {
        name = "over";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: b :: xs -> { state with stack = a :: b :: a :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TAny; TAny ];
        type_out = [ TAny; TAny; TAny ];
      };
      {
        name = "rot3";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: b :: c :: xs -> { state with stack = b :: c :: a :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TAny; TAny; TAny ];
        type_out = [ TAny; TAny; TAny ];
      };
      {
        name = "rot4";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: b :: c :: d :: xs -> { state with stack = b :: c :: d :: a :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TAny; TAny; TAny; TAny ];
        type_out = [ TAny; TAny; TAny; TAny ];
      };
      {
        name = "print";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: xs ->
                  print_endline @@ stack_value_to_string a;
                  { state with stack = xs }
              | _ -> raise @@ Failure "Stack underflow");
        type_in = [ TAny ];
        type_out = [];
      };
      {
        name = "clear";
        impl = Builtin (fun state -> { state with stack = [] });
        type_in = [];
        type_out = [];
      }
    ]
  in
  List.fold_left (fun acc word -> SMap.add word.name word acc) SMap.empty words
