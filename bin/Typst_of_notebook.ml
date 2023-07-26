let header =
  {|

  #let bgcolor_code = luma(230)
  #let bgcolor_result = rgb("a7d1de")
  #let codeblock(
      bgcolor: luma(230),
      rawcode) = block(fill: bgcolor,
                    outset: 5pt,
                    radius: 3pt,
                    width: 100%,
                    rawcode)
  #let resultblock(bgcolor: white, stroke: 1pt + luma(150), content) = [
      #move(
          align(
              right, box(
                  inset: 0pt, height: 0pt, 
                  text(size: 10pt, fill: luma(140))[_Result:_])),
              dx: -4em, dy: 12pt)
      #block(fill: bgcolor, outset: 5pt, radius: 3pt, width: 100%, stroke: stroke, raw(content))
  ]
  
  |}

module Json = Yojson.Basic
open Notebook_parse
open Util
open Base

let cell_to_typst buf lang = function
  | Markdown md ->
      Buffer.add_string buf (Typst.markdown_to_typst md.source);
      Buffer.add_char buf '\n'
  | Code cd ->
      let s = Printf.sprintf "#codeblock([\n```%s\n%s\n```\n])" lang cd.source in
      Buffer.add_string buf s;
      Buffer.add_char buf '\n'
  | Raw r -> raise Unimplemented

let nb_to_typst nb =
  let lang =
    try
      Json_util.cast_string
      @@ Json_util.recursive_find (`Assoc nb.meta) [ "kernel_spec"; "language" ]
    with
    | Json_util.Json_key_exception _ ->
        Json_util.cast_string
        @@ Json_util.recursive_find (`Assoc nb.meta) [ "language_info"; "name" ]
    | e -> raise e
  in
  let buf = Buffer.create 4096 in
  let f = cell_to_typst buf lang in
  Buffer.add_string buf header;
  List.iter ~f nb.cells;
  Buffer.contents buf