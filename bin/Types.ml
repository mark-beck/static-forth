module SMap = Map.Make (String)

module Ftype = struct
  type t =
    | TAny
    | TGen of string
    | TWord
    | TNumber
    | TString
    | TBool
    | TList of t

  let eq t1 t2 = t1 = t2 || t1 = TAny || t2 = TAny
  let ( == ) = eq

  let gen_eq t1 t2 =
    match (t1, t2) with
    | _, TGen _ -> true
    | TGen _, _ -> true
    | _ -> eq t1 t2

  let rec to_string = function
    | TAny -> "TAny"
    | TGen s -> Printf.sprintf "TGen(%s)" s
    | TWord -> "TWord"
    | TNumber -> "TNumber"
    | TString -> "TString"
    | TBool -> "TBool"
    | TList t -> Printf.sprintf "TList(%s)" (to_string t)

  let subst (dict : t SMap.t) (t : t) =
    let open Result in
    let rec subst = function
      | TAny -> Ok TAny
      | TGen s -> (
          try ok @@ SMap.find s dict with Not_found -> Result.error @@ TGen s)
      | TWord -> Ok TWord
      | TNumber -> Ok TNumber
      | TString -> Ok TString
      | TBool -> Ok TBool
      | TList t -> bind (subst t) (fun t -> Ok (TList t))
    in
    subst t
end

module Vtype = struct
  type t = VNumber of int | VString of string | VBool of bool

  let eq t1 t2 =
    match (t1, t2) with
    | VNumber v1, VNumber v2 when v1 = v2 -> true
    | VString v1, VString v2 when v1 = v2 -> true
    | VBool v1, VBool v2 when v1 = v2 -> true
    | _ -> false

  let ( == ) = eq

  let to_string = function
    | VNumber n -> string_of_int n
    | VString s -> s
    | VBool b -> string_of_bool b
end
