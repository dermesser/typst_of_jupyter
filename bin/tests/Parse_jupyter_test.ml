module Parse_jupyter = Notebook_parse.Parse_jupyter
module Json = Yojson.Basic
open Base

let dump_sexp s = Out_channel.output_string Out_channel.stdout (Sexp.to_string_hum s)

let test_md_cell_1 =
  {|
{
"cell_type" : "markdown",
"metadata" : {},
"source" : "# Hello World\nmulti-line *markdown*"
}
|}

let%expect_test _ =
  let cell_json = Json.from_string test_md_cell_1 in
  let parsed = Parse_jupyter.Markdown.cell_of_json cell_json in
  let md = Parsexp.Single.parse_string_exn (Omd.to_sexp parsed.source) in
  dump_sexp [%sexp {meta = ((Json.show (`Assoc parsed.meta)) : string); source = (md: Sexp.t)}];
  [%expect
    {|
    ((meta "`Assoc ([])")
     (source
      ((heading 1 "Hello World")
       (paragraph (concat "multi-line " (emph markdown)))))) |}]

let test_raw_cell_1 = {|
{
  "cell_type" : "raw",
  "metadata" : {
    "format" : "mime/type"
  },
  "source" : "[some nbformat output text]"
}
|}

let%expect_test _ =
  let cell = Parse_jupyter.Raw.cell_of_json (Json.from_string test_raw_cell_1) in
  dump_sexp [%sexp { source = (cell.source : string); mime = (cell.mime : string); meta = ((Json.show (`Assoc cell.meta)):string) }];
  [%expect {|
    ((source "[some nbformat output text]") (mime mime/type)
     (meta "`Assoc ([(\"format\", `String (\"mime/type\"))])")) |}]

let test_code_cells =
  [
    {|{
    "output_type" : "stream",
    "name" : "stdout",
    "text" : "[multiline stream text]"
  }|};
    {|{
    "output_type" : "display_data",
    "data" : {
      "text/plain" : "[multiline text data]",
      "image/png": "[base64-encoded-multiline-png-data]",
      "application/json": {
        "key1": "data",
        "key2": ["some", "values"],
        "key3": {"more": "data"}
      },
      "application/vnd.exampleorg.type+json": {
        "key1": "data",
        "key2": ["some", "values"],
        "key3": {"more": "data"}
      }
    },
    "metadata" : {
      "image/png": {
        "width": 640,
        "height": 480
      }
    }
  }|};
    {|{
    "output_type" : "execute_result",
    "execution_count": 42,
    "data" : {
      "text/plain" : "[multiline text data]",
      "image/png": "[base64-encoded-multiline-png-data]",
      "application/json": {
        "json": "data"
      }
    },
    "metadata" : {
      "image/png": {
        "width": 640,
        "height": 480
      }
    }
  }
  |};
    {|

  {
      "output_type": "error",
      "ename" : "some-error",
      "evalue" : "some-value",
      "traceback" : ["Trace", "Trace", "Trace"]
  }
  
  |};
  ]

let%expect_test _ =
  let js_outputs = List.map ~f:Json.from_string test_code_cells in
  let outputs = Parse_jupyter.Code.parse_outputs js_outputs in
  let o = Out_channel.stdout in
  List.iter
    ~f:(fun s ->
      Out_channel.output_string o s;
      Out_channel.output_string o "\n")
    (List.map ~f:Parse_jupyter.Code.output_type outputs);
  [%expect {|
    stream
    display_data
    execute_result
    error |}]
