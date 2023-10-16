open Base
open Core
open Angstrom

type texpression =
  | Macro0 of string
  | Macro1 of string * texpression
  | Macro2 of string * texpression * texpression
  | Token of string
[@@deriving sexp]

let token_char c =
  (not (Char.is_whitespace c))
  && not (List.mem ~equal:Char.equal [ '{'; '}'; '['; ']'; '('; ')' ] c)

let token = take_while1 token_char >>| fun t -> Token t
let macroname = char '\\' *> take_while Char.is_alphanum
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
let zero_or_once p = choice [ (p >>| fun e -> [ e ]); return [] ]

let rec maybe_sep_by s p =
  let* e = option () s *> zero_or_once p in
  match e with
  | [] -> return []
  | l -> maybe_sep_by s p >>= fun m -> return @@ List.append l m

let expression =
  skip_whitespace
  *> fix (fun e -> choice [ block e; macro2 e; macro1 e; macro0; token ])
  <* skip_whitespace

let expressions =
  maybe_sep_by (satisfy Char.is_whitespace |> lift ignore) expression

let parse_expression = parse_string ~consume:Consume.All expression
let parse_expressions = parse_string ~consume:Consume.All expressions

let parse_and_dump expr =
  let dump t = sexp_of_texpression t |> Sexp.to_string in
  let e = parse_expressions expr in
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
      {|\macro{input1} + \underbrace{abc}_{def}|};
      {|\macro{input1} + \underbrace{\abc{xyz}}_{\def}|};
    ]
  in
  List.iter ~f:parse_and_dump exprs;
  [%expect
    {|
    (Macro1 macro(Token input))
    (Macro2 macro(Token input2)(Token input2))
    (Macro1 macro(Token input1))~~(Token input2)
    (Macro1 macro(Token input1))~~(Token +)~~(Macro1 underbrace(Token abc))~~(Token _)~~(Token def)
    (Macro1 macro(Token input1))~~(Token +)~~(Macro1 underbrace(Macro1 abc(Token xyz)))~~(Token _)~~(Macro0 def) |}]

let known_symbols = Set.of_list (module String) [ "alpha"; "beta"; "pi" ]
let token_to_typst t = if Set.mem known_symbols t then Some t else None

let texpression_to_typst = function
  (* By default, translate tokens (like operators, variables, etc.) literally. *)
  | Token t -> t
  (* Check if a macro is a greek letter/symbol/etc., and only take more complex measures if it is not. *)
  | Macro0 t -> (
      match token_to_typst t with
      | Some t -> t
      | None -> failwith (sprintf "unknown command %s" t))
  | Macro1 (macro, arg) -> failwith "unimplemented: macro1"
  | Macro2 (macro, arg1, arg2) -> failwith "unimplemented: macro2"

let%expect_test "tex-to-typst" =
  let exprs = [ {|\pi|}; {|\alpha |} ] in
  List.iter
    ~f:(fun e ->
      printf "%s\n"
        (texpression_to_typst (Result.ok_or_failwith @@ parse_expression e)))
    exprs;
  [%expect {| pi |}]
