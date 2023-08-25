open Base
open Core
module Json = Yojson.Basic

module JR = struct
  type op =
    | Key of string
    | Index of int
    | As_int
    | As_float
    | As_bool
    | As_string
    | As_list of op
    | As_dict
    | Both of op * op
    | Map
    | Cat
  [@@deriving sexp]

  type 'a t = Value of 'a | Error of op list

  let error_to_string l = Sexp.to_string (Sexp.List (List.map ~f:sexp_of_op l))
  let error_root e = Error [ e ]

  let annotate v e =
    match v with Value v -> Value v | Error ee -> Error (e :: ee)

  let value v = Value v
end

open JR

type doc = Json.t
type ('a, 'b) t = { f : 'a -> 'b JR.t; op : op }

let run (m : (doc, 'a) t) (x : doc) =
  match m.f x with
  | Error e ->
      Result.Error (sprintf "element not found: %s" (error_to_string e))
  | Value y -> Result.Ok y

exception Json_object_error of string

let run_exn (m : (doc, 'a) t) (x : doc) =
  match m.f x with
  | Error e ->
      raise
        (Json_object_error (sprintf "element not found: %s" (error_to_string e)))
  | Value y -> y

let ( >> ) (a : ('a, 'b) t) (b : ('b, 'c) t) : ('a, 'c) t =
  let f x =
    match a.f x with Value v -> annotate (b.f v) a.op | Error e -> Error e
  in
  { f; op = Cat }

let ( >>? ) (a : ('a, 'b) t) (b : ('b JR.t, 'c) t) : ('a, 'c) t =
  let f x = b.f (a.f x) in
  { f; op = Cat }

let map (a : ('a, 'b) t) ~(f : 'b -> 'c) : ('a, 'c) t =
  let f x = match a.f x with Value y -> value (f y) | Error e -> Error e in
  { f; op = Map }

let int : (doc, int) t =
  let op = As_int in
  let f = function `Int i -> value i | _ -> error_root op in
  { f; op }

let float : (doc, float) t =
  let op = As_float in
  let f = function `Float f -> value f | _ -> error_root op in
  { f; op }

let string : (doc, string) t =
  let op = As_string in
  let f = function `String s -> value s | _ -> error_root op in
  { f; op }

let bool : (doc, bool) t =
  let op = As_bool in
  let f = function `Bool b -> value b | _ -> error_root op in
  { f; op }

let dict : (doc, doc) t =
  let op = As_dict in
  let f = function `Assoc a -> value (`Assoc a) | _ -> error_root op in
  { f; op }

(* Only returns a ['a list option] if all elements in list are of [typ]. *)
let list_of (typ : (doc, 'a) t) : (doc, 'a list) t =
  let op = As_list typ.op in
  let f = function
    | `List l ->
        let f a e =
          match (a, typ.f e) with
          | Value a, Value e -> value (e :: a)
          | Error e, Error e2 -> error_root op
          | Error e, _ -> Error e
          | _, Error e -> Error e
        in
        List.fold l ~init:(value []) ~f
    | _ -> error_root (As_list typ.op)
  in
  { f; op }

let list_filtered_of (typ : (doc, 'a) t) : (doc, 'a list) t =
  let op = As_list typ.op in
  let f = function
    | `List l ->
        let f a e =
          match (a, typ.f e) with
          | Value a, Value e -> value (e :: a)
          | Value a, Error _ -> value a
          | Error _, _ -> assert false
        in
        List.fold l ~init:(value []) ~f
    | _ -> error_root op
  in
  { f; op }

let key (k : string) (typ : (doc, 'a) t) : (doc, 'a) t =
  let op = Key k in
  let f = function
    | `Assoc a -> (
        match List.Assoc.find a ~equal:String.equal k with
        | Some v -> annotate (typ.f v) (Key k)
        | None -> error_root (Key k))
    | _ -> error_root op
  in
  { f; op }

let inner (k : string) : (doc, doc) t = key k dict

let both (a : ('i, 'a) t) (b : ('i, 'b) t) : ('i, 'a * 'b) t =
  let f x =
    match (a.f x, b.f x) with
    | Value a', Value b' -> value (a', b')
    | _, Error e | Error e, _ -> Error e
  in
  { f; op = Both (a.op, b.op) }

let either (a : ('i, 'a) t) (b : ('i, 'b) t) : ('i, ('a, 'b) Either.t) t =
  let op = Both (a.op, b.op) in
  let f x =
    match (a.f x, b.f x) with
    | Value a', _ -> value (Either.First a')
    | _, Value b' -> value (Either.Second b')
    | Error ea, Error eb -> error_root op
  in
  { f; op }

let alternative (a : ('i, 'a) t) (b : ('i, 'a) t) : ('i, 'a) t =
  let op = Both (a.op, b.op) in
  let f x =
    match (a.f x, b.f x) with
    | Value a', _ -> value a'
    | _, Value b' -> value b'
    | Error ea, _-> annotate (Error ea) op
  in
  { f; op }

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
  [%expect {| element not found: ((Key helloo)) |}]

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

let%expect_test "extract_dict_nested_path_error" =
  let got = run (path [ "dict"; "inner"; "key" ] int) test_doc in
  (match got with Ok _ -> printf "failure" | Error e -> printf "%s" e);
  [%expect {| element not found: ((Key dict)(Key inner)(Key key)As_int) |}]

let%expect_test "extract_both" =
  let got_a, got_b = run_exn (key "a" int <+> key "b" dict) test_doc in
  (match got_b with
  | `Assoc got_b -> printf "%d %d" got_a (List.length got_b)
  | _ -> printf "failure!");
  [%expect {| 1 0 |}]

let%expect_test "extract_both_error" =
  let got = run (key "a" int <+> key "c" dict) test_doc in
  (match got with Error e -> printf "%s" e | _ -> printf "failure!");
  [%expect {| element not found: ((Key c)) |}]

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
