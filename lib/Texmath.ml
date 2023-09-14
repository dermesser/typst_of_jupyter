open Base
open Core
open Angstrom

type texpression =
  | Macro0 of string
  | Macro1 of string * texpression
  | Macro2 of string * texpression * texpression
  | Token of string
[@@deriving sexp]

type result = Ok of texpression | Error of { pos : int; reason : string }

let failwith reason =
  let+ pos = pos in
  Error { pos; reason }

let chain f g x = f (g x)
let negate f = chain not f

let token_char c =
  (not (Char.is_whitespace c))
  && not (List.mem ~equal:Char.equal [ '{'; '}'; '['; ']'; '('; ')' ] c)

let token = take_while1 token_char >>| fun t -> Token t
let macroname = char '\\' *> take_while Char.is_alpha
let skip_whitespace = skip_while Char.is_whitespace

let rec block e = char '{' *> e <* char '}' <?> "block"

and macro1 e =
  let* mn = macroname in
  let+ arg = block e in
  Macro1 (mn, arg)

and macro2 e =
  let* mn = macroname in
  let* arg1 = block e in
  let+ arg2 = block e in
  Macro2 (mn, arg2, arg2)

let macro0 = macroname >>| fun mn -> Macro0 mn

let expression =
  fix (fun e -> choice [ block e; macro2 e; macro1 e; macro0; token ])

let expressions =
  sep_by1 (satisfy Char.is_whitespace) expression <* skip_whitespace

let parse_and_dump expr =
  let dump t = sexp_of_texpression t |> Sexp.to_string in
  let e = parse_string ~consume:Angstrom.Consume.All expressions expr in
  match e with
  | Ok ts ->
      Printf.printf "%s\n" (String.concat ~sep:"~~" (List.map ~f:dump ts))
  | Error s -> Printf.printf "error for \"%s\": %s\n" expr s

let%expect_test "simple-expression" =
  let exprs =
    [
      {|\macro{input}|};
      {|\macro{input1}{input2} |};
      {|\macro{input1} {input2}|};
      {|\macro{input1} + \underbrace{abc}{def}|};
    ]
  in
  List.iter ~f:parse_and_dump exprs;
  [%expect
    {|
    (Macro1 macro(Token input))
    (Macro2 macro(Token input2)(Token input2))
    (Macro1 macro(Token input1))~~(Token input2)
    (Macro1 macro(Token input1))~~(Token +)~~(Macro2 underbrace(Token def)(Token def)) |}]
