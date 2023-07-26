let header =
  {|
#let sanitize_markdown(md) = md.replace("#", "=").replace("= ", "=")

#let bgcolor_code = luma(230)
#let bgcolor_result = rgb("a7d1de")
#let codeblock(
    lang: "python",
    bgcolor: luma(230),
    code) = block(fill: bgcolor,
                  outset: 5pt,
                  radius: 3pt,
                  width: 100%,
                  raw(code, lang: lang))
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

open Util
open Base
open Omd

let inline_to_typst buf = function
  | Text (_attr, text) -> Buffer.add_string buf text
  | _ -> raise Unimplemented

let markdown_to_typst md =
  let buf = Buffer.create 1024 in
  let endl () = Buffer.add_char buf '\n' in
  (* Convert single block to typst by writing to buffer. *)
  let rec block_to_typst buf = function
    | Paragraph (attr, inl) ->
        inline_to_typst buf inl
    | List (_attr, lt, _ls, blocklist_list) ->
        let bullet =
          match lt with
          | Ordered (i, c) -> Printf.sprintf "%d%c " i c
          | Bullet c -> Char.to_string c
        in
        let write_listitem_block block =
          Buffer.add_string buf bullet;
          block_to_typst buf block;
          endl ()
        in
        let write_listitem = List.iter ~f:write_listitem_block in
        List.iter ~f:write_listitem blocklist_list
    | Blockquote (_attr, blocklist) ->
        (* todo: find best typst equivalent *)
        List.iter ~f:(block_to_typst buf) blocklist
    | Thematic_break attr ->
        Buffer.add_string buf "#line(length: 100%, stroke: gray)\n"
    | Heading (attr, level, inl) ->
        let marker =
          String.concat
            (Sequence.to_list (Sequence.take (Sequence.repeat "=") level))
        in
    endl ();
        Buffer.add_string buf marker;
        Buffer.add_string buf " ";
        inline_to_typst buf inl;
        endl ()
    | Code_block (attr, lang, contents) ->
        Buffer.add_string buf "```";
        Buffer.add_string buf lang;
        Buffer.add_char buf '\n';
        Buffer.add_string buf contents;
        Buffer.add_string buf "```";
    | Html_block (attr, s) -> raise Unimplemented
    | Definition_list (attr, defs) -> raise Unimplemented
    | Table (attr, head, cells) -> raise Unimplemented
  in
  List.iter
    ~f:(fun block ->
      block_to_typst buf block;
      endl ())
    md;
  Buffer.add_char buf '\n';
  Buffer.contents buf
