open Types
module SMap = Map.Make (String)

type implementation =
  | Builtin of (state -> state)
  | User of Parser.astnode list

and state = { stack : Vtype.t list; dictionary : dictionary_value SMap.t }

and dictionary_value = {
  name : string;
  impl : implementation;
  in_types : Ftype.t list;
  out_types : Ftype.t list;
}

let print_stack stack =
  let joined = String.concat ", " (List.map Vtype.to_string stack) in
  Printf.printf "Stack: [ %s ]" joined

let add_word (worddef : Parser.worddef) state =
  let word =
    {
      name = worddef.name;
      impl = User worddef.nodes;
      in_types = worddef.types_in;
      out_types = worddef.types_out;
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
        in_types = [];
        out_types = [ TBool ];
      };
      {
        name = "F";
        impl =
          Builtin
            (fun state -> { state with stack = VBool false :: state.stack });
        in_types = [];
        out_types = [ TBool ];
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
        in_types = [ TNumber; TNumber ];
        out_types = [ TNumber ];
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
        in_types = [ TNumber; TNumber ];
        out_types = [ TNumber ];
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
        in_types = [ TNumber; TNumber ];
        out_types = [ TNumber ];
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
        in_types = [ TNumber; TNumber ];
        out_types = [ TNumber ];
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
        in_types = [ TNumber; TNumber ];
        out_types = [ TBool ];
      };
      {
        name = "not";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | VBool a :: xs -> { state with stack = VBool (not a) :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        in_types = [ TBool ];
        out_types = [ TBool ];
      };
      {
        name = "dup";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | x :: xs -> { state with stack = x :: x :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        in_types = [ TGen "a" ];
        out_types = [ TGen "a"; TGen "a" ];
      };
      {
        name = "drop";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | _ :: xs -> { state with stack = xs }
              | _ -> raise @@ Failure "Stack underflow");
        in_types = [ TAny ];
        out_types = [];
      };
      {
        name = "swap";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: b :: xs -> { state with stack = b :: a :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        in_types = [ TGen "a"; TGen "b" ];
        out_types = [ TGen "b"; TGen "a" ];
      };
      {
        name = "over";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: b :: xs -> { state with stack = a :: b :: a :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        in_types = [ TGen "a"; TGen "b" ];
        out_types = [ TGen "a"; TGen "b"; TGen "a" ];
      };
      {
        name = "rot3";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: b :: c :: xs -> { state with stack = b :: c :: a :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        in_types = [ TGen "a"; TGen "b"; TGen "c" ];
        out_types = [ TGen "b"; TGen "c"; TGen "a" ];
      };
      {
        name = "rot4";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: b :: c :: d :: xs ->
                  { state with stack = b :: c :: d :: a :: xs }
              | _ -> raise @@ Failure "Stack underflow");
        in_types = [ TGen "a"; TGen "b"; TGen "c"; TGen "d" ];
        out_types = [ TGen "b"; TGen "c"; TGen "d"; TGen "a" ];
      };
      {
        name = "print";
        impl =
          Builtin
            (fun state ->
              match state.stack with
              | a :: xs ->
                  print_endline @@ Vtype.to_string a;
                  { state with stack = xs }
              | _ -> raise @@ Failure "Stack underflow");
        in_types = [ TAny ];
        out_types = [];
      };
      {
        name = "clear";
        impl = Builtin (fun state -> { state with stack = [] });
        in_types = [];
        out_types = [];
      };
    ]
  in
  List.fold_left (fun acc word -> SMap.add word.name word acc) SMap.empty words
