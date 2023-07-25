module Parse_jupyter = Notebook_parse.Parse_jupyter
module Json = Yojson.Basic

let test_md_cell_1 = {|
{
"cell_type" : "markdown",
"metadata" : {},
"source" : "# Hello World\nmulti-line *markdown*"
}
|}
let%expect_test _ =
  let cell_json = Json.from_string test_md_cell_1 in
  let parsed = Parse_jupyter.Markdown.cell_of_json cell_json in
  Out_channel.output_string Out_channel.stdout (Omd.to_sexp parsed.source);
  [%expect {|
    ((heading 1 "Hello World")
     (paragraph (concat "multi-line " (emph markdown)))) |}]
