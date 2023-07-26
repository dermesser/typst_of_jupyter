let header =
  {|

  #let bgcolor_code = luma(230)
  #let bgcolor_result = rgb("a7d1de")
  #let errorblock(content) = block(
    fill: rgb("#ffcccc"), outset: 5pt,
    radius: 3pt,
    width: 100%,
    content)
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
      #block(fill: bgcolor, outset: 5pt, radius: 3pt, width: 100%, stroke: stroke, content)
  ]
  
  |}

module Json = Yojson.Basic
open Notebook_parse
open Util
open Base

let output_to_typst buf = function
  | Code.ExecuteResult { data; meta } -> (
      match Json_util.find_assoc_opt data "text/plain" with
      | None -> ()
      | Some lines ->
          let lines = Json_util.cast_string_list lines in
          Buffer.add_string buf "```";
          List.iter ~f:(Buffer.add_string buf) lines;
          Buffer.add_string buf "```\n")
  | Code.ErrorOutput { ename; evalue; traceback } ->
      let s = "#errorblock([```\n" in
      Buffer.add_string buf s;
      Sequence.iter ~f:(Buffer.add_string buf)
        (Sequence.intersperse ~sep:"\n" @@ Sequence.of_list traceback);
      Buffer.add_string buf "\n```\n])"
  | _ -> raise Unimplemented

let cell_to_typst buf lang = function
  | Markdown md ->
      Buffer.add_string buf (Typst.markdown_to_typst md.source);
      Buffer.add_char buf '\n'
  | Code cd ->
      let source =
        let outbuf = Buffer.create (16 + String.length cd.source) in
        Buffer.add_string outbuf "```";
        Buffer.add_string outbuf lang;
        Buffer.add_char outbuf '\n';
        Buffer.add_string outbuf cd.source;
        Buffer.add_string outbuf "\n```\n";
        Buffer.contents outbuf
      in
      let output =
        if not (List.is_empty cd.outputs) then (
          let outbuf = Buffer.create 1024 in
          List.iter ~f:(output_to_typst outbuf) cd.outputs;
          Some (Buffer.contents outbuf))
        else None
      in
      let cell_text =
        Printf.sprintf
          {|
      #move(align(right, box(text([[%d]], fill: blue), fill: red, inset: 0pt, height: 0pt)), dx: -35pt, dy: 10pt)
      #codeblock([%s])|}
          cd.execount source
      in
      let result_box = match output with
      |  Some output -> Printf.sprintf "#resultblock([%s])" output
      | None -> "" in
      Buffer.add_string buf cell_text;
      Buffer.add_string buf result_box;
      Buffer.add_char buf '\n'
  | Raw r -> raise Unimplemented

let nb_to_typst nb =
  let lang =
    try
      Json_util.cast_string
      @@ Json_util.recursive_find_assoc_exn nb.meta
           [ "kernel_spec"; "language" ]
    with
    | Json_util.Json_key_exception _ ->
        Json_util.cast_string
        @@ Json_util.recursive_find_assoc_exn nb.meta
             [ "language_info"; "name" ]
    | e -> raise e
  in
  let buf = Buffer.create 4096 in
  let f = cell_to_typst buf lang in
  Buffer.add_string buf header;
  List.iter ~f nb.cells;
  Buffer.contents buf
