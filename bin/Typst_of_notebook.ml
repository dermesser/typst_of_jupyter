(* This is the style definition used by default. The generated cells are mostly using these definitions
   and are therefore independent of the final presentation. *)
module Json = Yojson.Basic
open Json_get
open Notebook
open Util
open Base
open Core.Printf

module Context = struct
  (* Configuration of rendering process, and buffer for result. *)
  type ctx = { asset_path : string; buf : Buffer.t }

  let create asset_path = { asset_path; buf = Buffer.create 1024 }
end

module Render = struct
  (** Logic for managing rendering results. Note that the actual text is
      stored in a mutable buffer handled by the Context module. *)

  (* Result from rendering a cell. *)
  type render = { attachments : Markdown.attachments }

  let merge a b = { attachments = List.append a.attachments b.attachments }
  let empty = { attachments = [] }
end

open Context

(* Strip ANSI escape codes. *)
let sanitize_text_plain s =
  let ps = Ansiparse.Concrete.parse_str s in
  let f = function
    | Ansiparse.Concrete.Esc _ -> ""
    | Ansiparse.Concrete.Reset -> ""
    | Ansiparse.Concrete.Text t -> t
  in
  String.concat (List.map ~f ps)

(* Converts a dict of mime type -> base64 contents to an attachments list,
   or writes it inline to the buffer at the current position (plain text). *)
let extract_code_outputs ctx buf data =
  let _ =
    match Json_util.find_assoc_opt data "text/plain" with
    | None -> []
    | Some lines ->
        let lines = extract_exn (list_of string) lines in
        let lines = List.map ~f:sanitize_text_plain lines in
        Buffer.add_string buf "```";
        List.iter ~f:(Buffer.add_string buf) lines;
        Buffer.add_string buf "```\n";
        []
  in
  let format_attachment (mime, ext) =
    match Json_util.find_assoc_opt data mime with
    | None -> None
    | Some png ->
        let pngbin = Base64.decode_exn (extract_exn string png) in
        let filename = String.append (random_string ()) ext in
        Buffer.add_string buf (Printf.sprintf {|#pngimage("%s")|} filename);
        Some (filename, pngbin)
  in
  Util.mapfirst format_attachment
    [ ("image/png", ".png"); ("image/svg+xml", ".svg") ]

let output_to_typst ({ buf; _ } as ctx) = function
  | Code.ExecuteResult { data; meta } ->
      Option.to_list @@ extract_code_outputs ctx buf data
  | Code.ErrorOutput { ename; evalue; traceback } ->
      let s = "#errorblock([```\n" in
      Buffer.add_string buf s;
      Sequence.iter ~f:(Buffer.add_string buf)
        (Sequence.intersperse ~sep:"\n" @@ Sequence.of_list traceback);
      Buffer.add_string buf "\n```\n])";
      []
  | Code.DisplayData { data; meta } -> []
  | Code.Stream { name; text } -> []

let cell_to_typst ({ buf; _ } as ctx) lang = function
  | Markdown md ->
      let r = Typst.markdown_to_typst md.source in
      let attachments = md.attachments in
      Buffer.add_string buf r;
      { Render.attachments }
  | Code cd ->
      (* raw code block *)
      let source =
        let srcbuf = Buffer.create (16 + String.length cd.source) in
        Buffer.add_string srcbuf "```";
        Buffer.add_string srcbuf lang;
        Buffer.add_char srcbuf '\n';
        Buffer.add_string srcbuf cd.source;
        Buffer.add_string srcbuf "\n```\n";
        Buffer.contents srcbuf
      in
      (* result of cell: (contents, attachments) *)
      let result =
        if not (List.is_empty cd.outputs) then (
          let resultbuf = Buffer.create 1024 in
          Buffer.add_string resultbuf "#resultblock([";
          let attachments =
            List.map
              ~f:(output_to_typst { ctx with buf = resultbuf })
              cd.outputs
            |> List.concat
          in
          Buffer.add_string resultbuf "])";
          Some (Buffer.contents resultbuf, { Render.attachments }))
        else None
      in
      (* Render execution count as blue [number] and the code to the right of it. *)
      let cell_text =
        Printf.sprintf {|#exec_count("%d")
#codeblock([%s])
|} cd.execount
          source
      in
      (* Render the output, if there is output. *)
      let result_box, attachments =
        match result with
        | Some (text, output) -> (text, output.attachments)
        | None -> ("", [])
      in
      Buffer.add_string buf cell_text;
      Buffer.add_string buf result_box;
      Buffer.add_char buf '\n';
      { Render.attachments }
  | Raw r -> raise Unimplemented

let write_attachments ctx a =
  let f (name, data) =
    (* the file names must be complete paths (but can be relative or absolute) *)
    let write_file ch = Out_channel.output_string ch data in
    let path = Filename_base.concat ctx.asset_path name in
    Out_channel.with_open_bin path write_file
  in
  List.iter ~f a

let write_render ctx main_file { Render.attachments } text =
  let fn = Filename_base.concat ctx.asset_path main_file in
  write_attachments ctx attachments;
  let write_typst ch = Out_channel.output_string ch text in
  Out_channel.with_open_bin fn write_typst;
  fn

(* Create or use directory at [asset_path] and generate files there. *)
let nb_to_typst ?(asset_path = "typstofjupyter_assets") ~header
    ?(main_file = "main.typ") nb =
  let lang =
    match
      extract (path [ "kernel_spec"; "language" ] string) (`Assoc nb.meta)
    with
    | Error _ ->
        extract_exn (path [ "language_info"; "name" ] string) (`Assoc nb.meta)
    | Ok v -> v
  in
  let buf = Buffer.create 4096 in
  let ctx = { asset_path; buf } in
  let convert_cell = cell_to_typst ctx lang in
  (try Core_unix.mkdir asset_path with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | _ -> ());
  Buffer.add_string buf header;
  (* Concatenates all rendered cells. *)
  let render =
    List.fold ~init:Render.empty ~f:Render.merge
      (List.map ~f:convert_cell nb.cells)
  in
  printf "%d attachments\n" (List.length render.attachments);
  write_render ctx main_file render (Buffer.contents buf)
