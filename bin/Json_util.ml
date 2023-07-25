module Assoc = Base.List.Assoc
module Json = Yojson.Basic

let find_assoc ~default l key =
  Option.value ~default (Assoc.find ~equal:String.equal l key)

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

exception Json_type_exception of string

let raise_type_error expected got =
  let msg =
    Printf.sprintf "Expected %s but got %s" expected (Json.pretty_to_string got)
  in
  raise (Json_type_exception msg)

let cast_assoc = function `Assoc l -> l | x -> raise_type_error "dict" x
let cast_string = function `String s -> s | x -> raise_type_error "string" x
let%test "cast_string_ok" = String.equal "Hello" (cast_string (`String "Hello"))

let%test "cast_string_not_ok" =
  try String.equal "Hello" (cast_string (`Float 123.)) with
  | Json_type_exception e -> String.equal "Expected string but got 123.0" e
  | _ -> false

let%test "cast_assoc_ok" =
  match cast_assoc (`Assoc [ ("Hello", `String "World") ]) with
  | [ ("Hello", `String "World") ] -> true
  | _ -> false
