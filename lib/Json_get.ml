open Base
open Core
module Json = Yojson.Basic

type inp = Json.t
type ('a, 'b) t = 'a -> 'b option

let ( >> ) (m : ('a, 'b) t) (f : ('b, 'c) t) : ('a, 'c) t =
 fun x -> match m x with Some v -> f v | None -> None

let ( >>? ) m f x = f (m x)

let map (v : ('a, 'b) t) (f : 'b -> 'c) = function
  | x -> ( match v x with Some r -> Some (f r) | None -> None)

exception Json_object_error of string

let run m = function x -> m x

(* TODO: make errors more useful *)
let run_exn m x =
  match m x with
  | None -> raise (Json_object_error "element not found")
  | Some y -> y

let int : (inp, int) t = function `Int i -> Some i | _ -> None
let string : (inp, string) t = function `String s -> Some s | _ -> None
let dict : (inp, inp) t = function `Assoc a -> Some (`Assoc a) | _ -> None

(* Only returns a ['a list option] if all elements in list are of [typ]. *)
let list_of (typ : (inp, 'a) t) : (inp, 'a list) t = function
  | `List l ->
      let f a e =
        match (a, typ e) with
        | Some a, Some e -> Some (e :: a)
        | None, _ | _, None -> None
      in
      List.fold l ~init:(Some []) ~f
  | _ -> None

let list_filtered_of (typ : (inp, 'a) t) : (inp, 'a list) t = function
  | `List l ->
      let f a e =
        match (a, typ e) with
        | Some a, Some e -> Some (e :: a)
        | Some a, None -> Some a
        | None, _ -> assert false
      in
      List.fold l ~init:(Some []) ~f
  | _ -> None

let key (k : string) (typ : (inp, 'a) t) : (inp, 'a) t = function
  | `Assoc a ->
      let open Option in
      List.Assoc.find a ~equal:String.equal k >>= typ
  | _ -> None

let rec path (keys : string list) (el : (inp, 'a) t) : (inp, 'a) t =
  match keys with
  | [] -> assert false
  | [ k ] -> key k el
  | k :: ks -> key k dict >> path ks el

let test_doc =
  Json.from_string
    {|{"hello": "world", "foo": ["bar", "baz"], "a": 1, "b": {}, "dict": {"inner": {"key": "value"}, "second": 123}}|}

let%expect_test "extract_dict" =
  let got = run_exn (key "hello" string) test_doc in
  Printf.printf "%s" got;
  [%expect {| world |}]

let%expect_test "extract_dict_nested" =
  let got = run_exn (key "dict" dict >> key "second" int) test_doc in
  Printf.printf "%d" got;
  [%expect {| 123 |}]

let%expect_test "extract_dict_list" =
  let got = run_exn (key "foo" (list_of string)) test_doc in
  Printf.printf "%d" (List.length got);
  [%expect {| 2 |}]


let%expect_test "extract_dict_nested_double" =
  let got =
    run_exn (key "dict" dict >> key "inner" dict >> key "key" string) test_doc
  in
  Printf.printf "%s" got;
  [%expect {| value |}]

let%expect_test "extract_dict_nested_path" =
  let got = run_exn (path [ "dict"; "inner"; "key" ] string) test_doc in
  Printf.printf "%s" got;
  [%expect {| value |}]
