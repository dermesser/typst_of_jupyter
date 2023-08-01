(* This is the style definition used by default. The generated cells are mostly using these definitions
   and are therefore independent of the final presentation. *)
let header =
  {|

  // Global style:
  #set text(font: "DejaVu Sans")

  // Layout functions (these can be adapted as long as the interface remains stable):
  #let bgcolor_code = luma(230)
  #let bgcolor_result = rgb("a7d1de")
  #let errorblock(content) = block(
    fill: rgb("#ffcccc"), outset: 5pt,
    radius: 3pt,
    width: 100%,
    content)
  #let exec_count(t) = move(align(right, box(text(fill: blue)[
    [#t]
  ], fill: red, inset: 0pt, height: 0pt)), dx: -35pt, dy: 10pt)
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
  #let pngimage(path) = image(path, width: 80%)

  |}

module Json = Yojson.Basic
open Notebook_parse
open Util
open Base
open Core.Printf

(* Where attachments etc. will be placed. *)
type ctx = { asset_path : string }

module Render = struct
  (* A list of attachments from execution results or markdown cells. *)
  type attachments = (string, string) List.Assoc.t

  (* Result from rendering a cell. *)
  type render = { attachments : attachments }

  let merge a b = { attachments = List.append a.attachments b.attachments }
  let empty = { attachments = [] }
end

let random_string ?(n = 12) () =
  let random_char () =
    Option.value ~default:'x' (Char.of_int (97 + Random.int 26))
  in
  String.of_char_list
    (Sequence.fold (Sequence.range 0 n) ~init:[] ~f:(fun xs _ ->
         random_char () :: xs))

(* converts a dict of mime type -> base64 contents to an attachments list, or writes it inline to the buffer at the current position (plain text). *)
let extract_code_outputs ctx buf data =
  let _ =
    match Json_util.find_assoc_opt data "text/plain" with
    | None -> []
    | Some lines ->
        let lines = Json_util.cast_string_list lines in
        Buffer.add_string buf "```";
        List.iter ~f:(Buffer.add_string buf) lines;
        Buffer.add_string buf "```\n";
        []
  in
  let png_attachment =
    match Json_util.find_assoc_opt data "image/png" with
    | None -> []
    | Some png ->
        let pngbin = Base64.decode_exn (Json_util.cast_string png) in
        let filename = String.append (random_string ()) ".png" in
        Buffer.add_string buf (Printf.sprintf {|#pngimage("%s")|} (Filename_base.concat ctx.asset_path filename));
        [ (filename, pngbin) ]
  in
  png_attachment

let output_to_typst ctx buf = function
  | Code.ExecuteResult { data; meta } -> extract_code_outputs ctx buf data
  | Code.ErrorOutput { ename; evalue; traceback } ->
      let s = "#errorblock([```\n" in
      Buffer.add_string buf s;
      Sequence.iter ~f:(Buffer.add_string buf)
        (Sequence.intersperse ~sep:"\n" @@ Sequence.of_list traceback);
      Buffer.add_string buf "\n```\n])";
      []
  | _ -> raise Unimplemented

(* Decode an attachment and write it to the given file. Automatically finds the correct mime entry.

   [data] is an assoc list of file contents keyed by mime type.
*)
let decode_attachment filename data =
  let find mime =
    (* Yojson.Basic.to_channel Out_channel.stdout (`Assoc data); *)
    match Json_util.find_assoc_opt data mime with
    | None -> []
    | Some contents ->
        (* Yojson.Basic.to_channel Out_channel.stdout contents; *)
        let bin = Base64.decode_exn (Json_util.cast_string contents) in
        [ (filename, bin) ]
  in
  let mimes = [ "image/png"; "image/svg+xml" ] in
  (* TODO: expand list *)
  (* Only write first found file type. *)
  let f l mime = match l with [] -> find mime | x -> x in
  List.fold ~init:[] ~f mimes

let extract_markdown_attachments attachments =
  let extract_attachment = function
    | filename, `Assoc data -> decode_attachment filename data
    | _ -> raise (Json_error "unexpected attachment format in markdown cell")
  in
  List.concat_map ~f:extract_attachment attachments

let cell_to_typst ctx buf lang = function
  | Markdown md ->
      let r = Typst.markdown_to_typst md.source in
      let attachments = extract_markdown_attachments md.attachments in
      Buffer.add_string buf r;
      Buffer.add_char buf '\n';
      { Render.attachments }
  | Code cd ->
      (* raw code block *)
      let source =
        let outbuf = Buffer.create (16 + String.length cd.source) in
        Buffer.add_string outbuf "```";
        Buffer.add_string outbuf lang;
        Buffer.add_char outbuf '\n';
        Buffer.add_string outbuf cd.source;
        Buffer.add_string outbuf "\n```\n";
        Buffer.contents outbuf
      in
      (* output of cell *)
      let output =
        if not (List.is_empty cd.outputs) then (
          let outbuf = Buffer.create 1024 in
          Buffer.add_string outbuf "#resultblock([";
          let attachments =
            List.map ~f:(output_to_typst ctx outbuf) cd.outputs |> List.concat
          in
          Buffer.add_string outbuf "])";
          Some (Buffer.contents outbuf, { Render.attachments }))
        else None
      in
      (* Render execution count as blue [number] and the code to the right of it. *)
      let cell_text =
        Printf.sprintf
          {|
      #exec_count("%d")
      #codeblock([%s])
          |}
          cd.execount source
      in
      (* Render the output, if there is output. *)
      let result_box, attachments =
        match output with
        | Some (text, output) -> (text, output.attachments)
        | None -> ("", [])
      in
      Buffer.add_string buf cell_text;
      Buffer.add_string buf result_box;
      Buffer.add_char buf '\n';
      { Render.attachments }
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
  let ctx = { asset_path = Filename_unix.temp_dir "typstofjup" "" } in
  let f = cell_to_typst ctx buf lang in
  Buffer.add_string buf header;
  let render =
    List.fold ~init:Render.empty ~f:Render.merge (List.map ~f nb.cells)
  in
  (* Make asset paths relative. *)
  let attachments =
    List.map
      ~f:(fun (s, d) -> (Filename_base.concat ctx.asset_path s, d))
      render.attachments
  in
  printf "%d attachments\n" (List.length attachments);
  (Buffer.contents buf, { Render.attachments })

let write_attachments a =
  let f (name, data) =
    (* the file names must be complete paths (but can be relative or absolute) *)
    let write_file ch = Out_channel.output_string ch data in
    Out_channel.with_open_bin name write_file
  in
  List.iter ~f a

let write_render { Render.attachments } text =
  let fn = Filename_unix.temp_file "typstofjup" ".typ" in
  write_attachments attachments;
  let write_typst ch = Out_channel.output_string ch text in
  Out_channel.with_open_bin fn write_typst;
  fn
