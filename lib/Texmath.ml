open Base
open Core
open Angstrom

type texpression =
  | Macro0 of string
  | Macro1 of string * texpression
  | Macro2 of string * texpression * texpression
  | Operator of texpression * char * texpression
  | Token of string
[@@deriving sexp]

let chain f g x = f (g x)
let negate f = chain not f

let token_char c =
  (not (Char.is_whitespace c))
  && not (List.mem ~equal:Char.equal [ '{'; '}'; '['; ']'; '('; ')' ] c)

let token = take_while token_char >>| fun t -> Token t
let macroname = char '\\' *> take_while Char.is_alpha

let rec block e = char '{' *> e <* char '}'

and macro1 e =
  let* mn = macroname in
  let+ arg = block e in
  Macro1 (mn, arg)

let macro0 = macroname >>| fun mn -> Macro0 mn
let expression = fix (fun e -> choice [ block e; macro1 e; macro0; token ])

let%expect_test "simple-expression" =
  match
    parse_string ~consume:Angstrom.Consume.All expression "\\hello{world}"
  with
  | Ok t -> Printf.printf "%s" (Sexp.to_string (sexp_of_texpression t))
  | Error s ->
      Printf.printf "error: %s" s;
      [%expect {| (Macro1 hello(Token world)) |}]
