open Base
open Core
module Json = Yojson.Basic

type doc = Json.t
type ('a, 'b) t = 'a -> 'b option

let ( >> ) (m : ('a, 'b) t) (f : ('b, 'c) t) : ('a, 'c) t =
 fun x -> match m x with Some v -> f v | None -> None

let ( >>? ) m f x = f (m x)

let map (v : ('a, 'b) t) ~(f : 'b -> 'c) : ('a, 'c) t = function
  | x -> ( match v x with Some r -> Some (f r) | None -> None)

exception Json_object_error of string

let run m = function x -> m x

(* TODO: make errors more useful *)
let run_exn m x =
  match m x with
  | None -> raise (Json_object_error "element not found")
  | Some y -> y

let int : (doc, int) t = function `Int i -> Some i | _ -> None
let string : (doc, string) t = function `String s -> Some s | _ -> None
let dict : (doc, doc) t = function `Assoc a -> Some (`Assoc a) | _ -> None

(* Only returns a ['a list option] if all elements in list are of [typ]. *)
let list_of (typ : (doc, 'a) t) : (doc, 'a list) t = function
  | `List l ->
      let f a e =
        match (a, typ e) with
        | Some a, Some e -> Some (e :: a)
        | None, _ | _, None -> None
      in
      List.fold l ~init:(Some []) ~f
  | _ -> None

let list_filtered_of (typ : (doc, 'a) t) : (doc, 'a list) t = function
  | `List l ->
      let f a e =
        match (a, typ e) with
        | Some a, Some e -> Some (e :: a)
        | Some a, None -> Some a
        | None, _ -> assert false
      in
      List.fold l ~init:(Some []) ~f
  | _ -> None

let key (k : string) (typ : (doc, 'a) t) : (doc, 'a) t = function
  | `Assoc a ->
      let open Option in
      List.Assoc.find a ~equal:String.equal k >>= typ
  | _ -> None

let inner (k : string) : (doc, doc) t = key k dict

let both (a : ('i, 'a) t) (b : ('i, 'b) t) : ('i, 'a * 'b) t =
 fun x -> match (a x, b x) with Some a', Some b' -> Some (a', b') | _ -> None

let either (a : ('i, 'a) t) (b : ('i, 'b) t) : ('i, ('a, 'b) Either.t) t =
 fun x ->
  match (a x, b x) with
  | Some a', _ -> Some (Either.First a')
  | _, Some b' -> Some (Either.Second b')
  | _, _ -> None

let alternative (a : ('i, 'a) t) (b : ('i, 'a) t) : ('i, 'a) t =
 fun x ->
  match (a x, b x) with
  | Some a', _ -> Some a'
  | _, Some b' -> Some b'
  | _ -> None

let ( <+> ) = both
let ( <|*> ) = either
let ( <|> ) = alternative

let rec path (keys : string list) (el : (doc, 'a) t) : (doc, 'a) t =
  match keys with
  | [] -> assert false
  | [ k ] -> key k el
  | k :: ks -> inner k >> path ks el

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
