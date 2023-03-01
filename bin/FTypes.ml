module SMap = Map.Make (String)

type ftype =
  | TAny
  | TWord
  | TNumber
  | TString
  | TBool
  | TList of ftype
  | TNever
  | TUnit

type stack_value = VNumber of int | VString of string | VBool of bool

let stack_value_to_string = function
  | VNumber n -> string_of_int n
  | VString s -> s
  | VBool b -> string_of_bool b
