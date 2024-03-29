(** Converting Markdown to Typst. Notebook conversion is implemented in Typst_of_notebook. *)

open Util
open Base
open Omd

exception Markdown_to_typst_mismatch of string

let rec inline_to_typst buf =
  let s str = Buffer.add_string buf str in
  let c ch = Buffer.add_char buf ch in
  function
  | Text (_attr, text) ->
      let escaped_txt = Str.global_replace (Str.regexp "\\$\\") "$" text in
      let escaped_txt = Str.global_replace (Str.regexp "@") "\\@" escaped_txt in
      s escaped_txt
  | Concat (_attr, inls) -> List.iter ~f:(inline_to_typst buf) inls
  | Image (_attr, { label; destination; title }) ->
      let label =
        let buf2 = Buffer.create 512 in
        inline_to_typst buf2 label;
        Buffer.contents buf2
      in
      let destination =
        String.chop_prefix_if_exists destination ~prefix:"attachment:"
      in
      let figstr =
        Printf.sprintf {|#figure(image("%s", width: 80%%), caption: "%s")|}
          destination label
      in
      s figstr
  | Link (_attr, { label; destination; title }) ->
      let label =
        let buf2 = Buffer.create 512 in
        inline_to_typst buf2 label;
        Buffer.contents buf2
      in
      let linkstr = Printf.sprintf {|#link("%s")[%s]|} destination label in
      s linkstr
  | Html _ ->
      raise
        (Markdown_to_typst_mismatch
           "HTML content cannot be easily converted to Typst markup.")
  | Emph (_attr, inl) ->
      c '_';
      inline_to_typst buf inl;
      c '_'
  | Strong (_attr, inl) ->
      c '*';
      inline_to_typst buf inl;
      c '*'
  | Code _ -> ()
  | Hard_break _ -> ()
  | Soft_break _ -> ()

let markdown_to_typst (md : doc) =
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
    | Thematic_break attr -> s "#line(length: 90%, stroke: gray)\n"
    | Heading (attr, level, inl) ->
        let marker =
          String.concat
            (Sequence.to_list (Sequence.take (Sequence.repeat "=") level))
        in
        endl ();
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
    | Html_block (attr, s) ->
        raise
          (Markdown_to_typst_mismatch
             "HTML content cannot be easily converted to Typst markup.")
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

let markdown_string_to_typst s =
  let doc = Omd.of_string s in
  markdown_to_typst doc
