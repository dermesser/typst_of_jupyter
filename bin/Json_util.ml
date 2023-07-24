module Assoc = Base.List.Assoc
module Json = Yojson.Basic

let find_assoc ~default l key = Option.value ~default (Assoc.find ~equal:String.equal l key)

exception Json_type_exception of string

let raise_type_error expected got =
  let msg = Printf.sprintf "Expected %s but got %s" expected (Json.pretty_to_string got) in
  raise (Json_type_exception msg)


let cast_assoc = function
  | `Assoc l -> l
  | x -> raise_type_error "dict" x
let cast_string = function
  | `String s -> s
  | x -> raise_type_error "string" x