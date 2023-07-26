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

let rec inline_to_typst buf = function
  | Text (_attr, text) -> Buffer.add_string buf text
  | Concat (_attr, inls) -> List.iter ~f:(inline_to_typst buf) inls
  | Image (_attr, { label; destination; title }) ->
      let label =
        let buf2 = Buffer.create 512 in
        inline_to_typst buf2 label;
        Buffer.contents buf2
      in
      let s =
        Printf.sprintf {|#figure(image("%s", width: 80%%), caption: "%s")
        |}
          destination label
      in
      Buffer.add_string buf s
  | Link (_attr, { label; destination; title }) ->
      let label =
        let buf2 = Buffer.create 512 in
        inline_to_typst buf2 label;
        Buffer.contents buf2
      in
      let s = Printf.sprintf {|#link("%s")[%s]
      |} destination label in
      Buffer.add_string buf s
  | _ -> raise Unimplemented

let markdown_to_typst md =
  let buf = Buffer.create 1024 in
  let endl () = Buffer.add_char buf '\n' in
  let s str = Buffer.add_string buf str in
  let c ch = Buffer.add_char buf ch in
  (* Convert single block to typst by writing to buffer. *)
  let rec block_to_typst buf = function
    | Paragraph (attr, inl) -> inline_to_typst buf inl
    | List (_attr, lt, _ls, blocklist_list) ->
        let bullet =
          match lt with
          | Ordered (i, c) -> Printf.sprintf "%d%c " i c
          | Bullet c -> Printf.sprintf "%c " c
        in
        let write_listitem_block block =
          s bullet;
          block_to_typst buf block;
          endl ()
        in
        let write_listitem = List.iter ~f:write_listitem_block in
        List.iter ~f:write_listitem blocklist_list
    | Blockquote (_attr, blocklist) ->
        (* todo: find best typst equivalent *)
        List.iter ~f:(block_to_typst buf) blocklist
    | Thematic_break attr -> s "#line(length: 100%, stroke: gray)\n"
    | Heading (attr, level, inl) ->
        let marker =
          String.concat
            (Sequence.to_list (Sequence.take (Sequence.repeat "=") level))
        in
        s marker;
        s " ";
        inline_to_typst buf inl;
        endl ()
    | Code_block (attr, lang, contents) ->
        endl ();
        s "```";
        s lang;
        c '\n';
        s contents;
        s "```"
    | Html_block (attr, s) -> raise Unimplemented
    | Definition_list (_attr, defs) ->
        let write_term { term; defs } =
          (* assert (Int.equal 1 (List.length defs)); *)
          s "/ ";
          inline_to_typst buf term;
          s ": ";
          inline_to_typst buf (List.hd_exn defs);
          endl ()
        in
        List.iter ~f:write_term defs;
        endl ()
    | Table (attr, head, cells) -> raise Unimplemented
  in
  List.iter
    ~f:(fun block ->
      block_to_typst buf block;
      endl ())
    md;
  c '\n';
  Buffer.contents buf
