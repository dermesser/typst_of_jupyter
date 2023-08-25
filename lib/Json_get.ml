open Base
open Core
module Json = Yojson.Basic

module JR = struct
  type op =
    | Get_field of string
    | Get_element of int
    | Cast_int
    | Cast_float
    | Cast_bool
    | Cast_string
    | Cast_list
    | Cast_dict
    | Both of op list * op list
    | Map
  [@@deriving sexp]

  type 'a t = Value of 'a | Error of op list

  let error_to_string l = Sexp.to_string (Sexp.List (List.map ~f:sexp_of_op l))
  let error_root e = Error [ e ]
  let value v = Value v
end

open JR

type doc = Json.t
type ('a, 'b) t = { f : 'a -> 'b JR.t }

let run (m : (doc, 'a) t) (x : doc) =
  match m.f x with
  | Error e ->
      Result.Error (sprintf "element not found: %s" (error_to_string e))
  | Value y -> Result.Ok y

exception Json_object_error of string

(* TODO: make errors more useful *)
let run_exn m x =
  match m.f x with
  | Error e ->
      raise
        (Json_object_error (sprintf "element not found: %s" (error_to_string e)))
  | Value y -> y

let ( >> ) (a : ('a, 'b) t) (b : ('b, 'c) t) : ('a, 'c) t =
  let f x = match a.f x with Value v -> b.f v | Error e -> Error e in
  { f }

let ( >>? ) (a : ('a, 'b) t) (b : ('b JR.t, 'c) t) : ('a, 'c) t =
  let f x = b.f (a.f x) in
  { f }

let map (a : ('a, 'b) t) ~(f : 'b -> 'c) : ('a, 'c) t =
  let f x = match a.f x with Value y -> value (f y) | Error e -> Error e in
  { f }

let int : (doc, int) t =
  let f = function `Int i -> value i | _ -> error_root Cast_int in
  { f }

let float : (doc, float) t =
  let f = function `Float f -> value f | _ -> error_root Cast_float in
  { f }

let string : (doc, string) t =
  let f = function `String s -> value s | _ -> error_root Cast_string in
  { f }

let bool : (doc, bool) t =
  let f = function `Bool b -> value b | _ -> error_root Cast_bool in
  { f }

let dict : (doc, doc) t =
  let f = function `Assoc a -> value (`Assoc a) | _ -> error_root Cast_dict in
  { f }

(* Only returns a ['a list option] if all elements in list are of [typ]. *)
let list_of (typ : (doc, 'a) t) : (doc, 'a list) t =
  let f = function
    | `List l ->
        let f a e =
          match (a, typ.f e) with
          | Value a, Value e -> value (e :: a)
          | Error _, _ | _, Error _ -> Error []
        in
        List.fold l ~init:(value []) ~f
    | _ -> error_root Cast_int
  in
  { f }

let list_filtered_of (typ : (doc, 'a) t) : (doc, 'a list) t =
  let f = function
    | `List l ->
        let f a e =
          match (a, typ.f e) with
          | Value a, Value e -> value (e :: a)
          | Value a, Error _ -> value a
          | Error _, _ -> assert false
        in
        List.fold l ~init:(value []) ~f
    | _ -> error_root Cast_list
  in
  { f }

let key (k : string) (typ : (doc, 'a) t) : (doc, 'a) t =
  let f = function
    | `Assoc a -> (
        match List.Assoc.find a ~equal:String.equal k with
        | Some v -> typ.f v
        | None -> error_root (Get_field k))
    | _ -> error_root Cast_dict
  in
  { f }

let inner (k : string) : (doc, doc) t = key k dict

let both (a : ('i, 'a) t) (b : ('i, 'b) t) : ('i, 'a * 'b) t =
  let f x =
    match (a.f x, b.f x) with
    | Value a', Value b' -> value (a', b')
    | Error ea, Error eb -> error_root (Both (ea, eb))
    | _, Error e | Error e, _ -> Error e
  in
  { f }

let either (a : ('i, 'a) t) (b : ('i, 'b) t) : ('i, ('a, 'b) Either.t) t =
  let f x =
    match (a.f x, b.f x) with
    | Value a', _ -> value (Either.First a')
    | _, Value b' -> value (Either.Second b')
    | Error ea, Error eb -> error_root (Both (ea, eb))
  in
  { f }

let alternative (a : ('i, 'a) t) (b : ('i, 'a) t) : ('i, 'a) t =
  let f x =
    match (a.f x, b.f x) with
    | Value a', _ -> value a'
    | _, Value b' -> value b'
    | Error ea, Error eb -> error_root (Both (ea, eb))
  in
  { f }

let ( <+> ) = both
let ( <|*> ) = either
let ( <|> ) = alternative

let rec path (keys : string list) (el : (doc, 'a) t) : (doc, 'a) t =
  match keys with
  | [] -> assert false
  | [ k ] -> key k el
  | k :: ks -> inner k >> path ks el

(* This is not super useful... *)
module Let_syntax = struct
  module Let_syntax = struct
    let return x _ = x
    let map = map
    let both = both

    module Open_on_rhs = struct end
  end
end

let test_doc =
  Json.from_string
    {|{"hello": "world", "foo": ["bar", "baz"], "a": 1, "b": {}, "dict": {"inner": {"key": "value"}, "second": 123}}|}

let%expect_test "extract_dict" =
  let got = run_exn (key "hello" string) test_doc in
  printf "%s" got;
  [%expect {| world |}]

let%expect_test "extract_dict_fail" =
  (match run (key "helloo" string) test_doc with
    | Error e -> printf "%s" e
    | Ok _ -> ());
  [%expect {| element not found: ((Get_field helloo)) |}]


let%expect_test "extract_dict_nested" =
  let got = run_exn (inner "dict" >> key "second" int) test_doc in
  printf "%d" got;
  [%expect {| 123 |}]

let%expect_test "extract_dict_list" =
  let got = run_exn (key "foo" (list_of string)) test_doc in
  printf "%d" (List.length got);
  [%expect {| 2 |}]

let%expect_test "extract_dict_nested_double" =
  let got =
    run_exn (inner "dict" >> inner "inner" >> key "key" string) test_doc
  in
  printf "%s" got;
  [%expect {| value |}]

let%expect_test "extract_dict_nested_path" =
  let got = run_exn (path [ "dict"; "inner"; "key" ] string) test_doc in
  printf "%s" got;
  [%expect {| value |}]

let%expect_test "extract_both" =
  let got_a, got_b = run_exn (key "a" int <+> key "b" dict) test_doc in
  (match got_b with
  | `Assoc got_b -> printf "%d %d" got_a (List.length got_b)
  | _ -> printf "failure!");
  [%expect {| 1 0 |}]

let%test "extract_either" =
  match run_exn (key "z" string <|*> key "a" int) test_doc with
  | Either.Second 1 -> true
  | Either.First _ -> false
  | _ -> false

let%test "extract_let_syntax" =
  let open Let_syntax in
  let ex =
    let%map v1 = key "a" int and v2 = key "b" dict in
    (v1, v2)
  in
  match run_exn ex test_doc with 1, `Assoc [] -> true | _ -> false
