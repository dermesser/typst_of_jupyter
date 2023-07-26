module Assoc = Base.List.Assoc
module Json = Yojson.Basic

open Base

exception Json_type_exception of string
exception Json_key_exception of string

let find_assoc ~default l key =
  Option.value ~default (Assoc.find ~equal:String.equal l key)

let find_assoc_exn l k = try List.Assoc.find_exn ~equal:String.equal l k with
  | Not_found_s _ -> raise (Json_key_exception k)

let%test "find_assoc" =
  let a =
    [ ("Key1", `Float 123.); ("Key2", `List [ `Int 1; `Int 2; `Int 3 ]) ]
  in
  match
    ( find_assoc ~default:(`Int 111) a "Key1",
      find_assoc ~default:(`Int 222) a "non-existent" )
  with
  | `Float 123., `Int 222 -> true
  | a, b ->
      let put = Out_channel.output_string Out_channel.stderr in
      put (Json.pretty_to_string a);
      put (Json.pretty_to_string b);
      false

let raise_type_error expected got =
  let msg =
    Printf.sprintf "Expected %s but got %s" expected (Json.pretty_to_string got)
  in
  raise (Json_type_exception msg)

let cast_assoc = function `Assoc l -> l | x -> raise_type_error "dict" x
let cast_string = function `String s -> s | x -> raise_type_error "string" x
let cast_string_list = function `List l -> List.map ~f:cast_string l | x -> raise_type_error "string list" x
let cast_int = function `Int i -> i | x -> raise_type_error "int" x
let cast_list = function `List l -> l | x -> raise_type_error "list" x

let%test "cast_string_ok" = String.equal "Hello" (cast_string (`String "Hello"))

let%test "cast_string_not_ok" =
  try String.equal "Hello" (cast_string (`Float 123.)) with
  | Json_type_exception e -> String.equal "Expected string but got 123.0" e
  | _ -> false

let%test "cast_assoc_ok" =
  match cast_assoc (`Assoc [ ("Hello", `String "World") ]) with
  | [ ("Hello", `String "World") ] -> true
  | _ -> false

let rec json_to_sexp = function
  | `Assoc l -> Sexp.List (List.map ~f:(fun ((k, v)) -> [%sexp (k:string), ((json_to_sexp v):Sexp.t)]) l)
  | `Bool b -> Bool.sexp_of_t b
  | `Float f -> Float.sexp_of_t f
  | `Int i -> Int.sexp_of_t i
  | `List l -> Sexp.List (List.map ~f:json_to_sexp l)
  | `Null -> Sexp.List []
  | `String s -> String.sexp_of_t s