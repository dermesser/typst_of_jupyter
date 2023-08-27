open Base
open Core
module Json = Yojson.Basic
module Json_util = Json_util

module JR = struct
  type op =
    | Key of string
    | Index of int
    | As_keys
    | As_values
    | As_assoc
    | As_int
    | As_float
    | As_bool
    | As_string
    | As_list of op
    | As_dict
    | Both of op * op
    | Map
    | Cat
    | Default
    | Function
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

let extract (m : (doc, 'a) t) (x : doc) =
  match m.f x with
  | Error e -> Result.Error (sprintf "json error: %s" (error_to_string e))
  | Value y -> Result.Ok y

let extract_or ~default (m : (doc, 'a) t) (x : doc) =
  match m.f x with Error e -> default | Value y -> y

exception Json_object_error of string

let extract_exn (m : (doc, 'a) t) (x : doc) =
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

let lift (f : 'a -> 'b) : ('a, 'b) t =
  let op = Function in
  let f = function x -> value (f x) in
  { f; op }

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

let assoc : (doc, (string, doc) List.Assoc.t) t =
  let op = As_assoc in
  let f = function `Assoc a -> value a | _ -> error_root op in
  { f; op }

let keys : (doc, string list) t =
  let op = As_keys in
  let f = function
    | `Assoc a -> value @@ List.map ~f:(fun (k, _) -> k) a
    | _ -> error_root op
  in
  { f; op }

let values : (doc, doc list) t =
  let op = As_values in
  let f = function
    | `Assoc a -> value @@ List.map ~f:(fun (_, v) -> v) a
    | _ -> error_root op
  in
  { f; op }

let dict : (doc, doc) t =
  let op = As_dict in
  let f = function `Assoc a -> value (`Assoc a) | _ -> error_root op in
  { f; op }

let list_index (ix : int) (typ : (doc, 'a) t) : (doc, 'a) t =
  let op = Index ix in
  let f = function
    | `List l -> (
        match List.nth l ix with Some e -> typ.f e | None -> error_root op)
    | _ -> error_root (As_list typ.op)
  in
  { f; op }

(* Only returns a ['a list option] if all elements in list are of [typ]. *)
let list_of (typ : (doc, 'a) t) : (doc, 'a list) t =
  let op = As_list typ.op in
  let f = function
    | `List l ->
        let f e a =
          match (a, typ.f e) with
          | Value a, Value e -> value (e :: a)
          | Error e, Error e2 -> error_root op
          | Error e, _ -> Error e
          | _, Error e -> Error e
        in
        List.fold_right l ~init:(value []) ~f
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
    | Error ea, _ -> annotate (Error ea) op
  in
  { f; op }

let ( <+> ) = both
let ( <|*> ) = either
let ( <|> ) = alternative

let default (default : 'd) : ('d JR.t, 'd) t =
  let op = Default in
  let f = function Value v -> value v | Error _ -> value default in
  { f; op }

let rec path (keys : string list) (el : (doc, 'a) t) : (doc, 'a) t =
  match keys with
  | [] -> assert false
  | [ k ] -> key k el
  | k :: ks -> inner k >> path ks el

let test_doc =
  Json.from_string
    {|{"hello": "world", "foo": ["bar", "baz"], "a": 1, "b": {}, "dict": {"inner": {"key": "value"}, "second": 123}}|}

let%expect_test "extract_dict" =
  let got = extract_exn (key "hello" string) test_doc in
  printf "%s" got;
  [%expect {| world |}]

let%expect_test "extract_dict_fail" =
  (match extract (key "helloo" string) test_doc with
  | Error e -> printf "%s" e
  | Ok _ -> ());
  [%expect {| json error: ((Key helloo)) |}]

let%expect_test "extract_dict_nested" =
  let got = extract_exn (inner "dict" >> key "second" int) test_doc in
  printf "%d" got;
  [%expect {| 123 |}]

let%expect_test "extract_dict_nested_assoc" =
  let got = extract_exn (inner "dict" >> key "inner" assoc) test_doc in
  let s = List.Assoc.sexp_of_t String.sexp_of_t Json_util.json_to_sexp got in
  printf "%s" (Sexp.to_string s);
  [%expect {| ((key value)) |}]

let%expect_test "extract_dict_list" =
  let got = extract_exn (key "foo" (list_of string)) test_doc in
  printf "%d" (List.length got);
  [%expect {| 2 |}]

let%expect_test "extract_dict_nested_double" =
  let got =
    extract_exn (inner "dict" >> inner "inner" >> key "key" string) test_doc
  in
  printf "%s" got;
  [%expect {| value |}]

let%expect_test "extract_dict_nested_path" =
  let got = extract_exn (path [ "dict"; "inner"; "key" ] string) test_doc in
  printf "%s" got;
  [%expect {| value |}]

let%expect_test "extract_dict_nested_path_error" =
  let got = extract (path [ "dict"; "inner"; "key" ] int) test_doc in
  (match got with Ok _ -> printf "failure" | Error e -> printf "%s" e);
  [%expect {| json error: ((Key dict)(Key inner)(Key key)As_int) |}]

let%expect_test "extract_both" =
  let got_a, got_b = extract_exn (key "a" int <+> key "b" dict) test_doc in
  (match got_b with
  | `Assoc got_b -> printf "%d %d" got_a (List.length got_b)
  | _ -> printf "failure!");
  [%expect {| 1 0 |}]

let%expect_test "extract_both_error" =
  let got = extract (key "a" int <+> key "c" dict) test_doc in
  (match got with Error e -> printf "%s" e | _ -> printf "failure!");
  [%expect {| json error: ((Key c)) |}]

let%test "extract_either" =
  match extract_exn (key "z" string <|*> key "a" int) test_doc with
  | Either.Second 1 -> true
  | Either.First _ -> false
  | _ -> false

let%expect_test "extract_default" =
  (* access ok*)
  printf "%d\n" (extract_exn (key "a" int >>? default 42) test_doc);
  (* cast error *)
  printf "%d\n" (extract_exn (key "b" int >>? default 42) test_doc);
  (* key error *)
  printf "%d\n" (extract_exn (key "c" int >>? default 42) test_doc);
  [%expect {|
    1
    42
    42 |}]
