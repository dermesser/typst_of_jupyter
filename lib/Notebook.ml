
open Util.Json_util
open! Util
open Base
module Json = Yojson.Basic
module Assoc = Base.List.Assoc

(* https://nbformat.readthedocs.io/en/latest/ *)

exception Json_error of string
exception File_format_error of Sexp.t

type metadata = (string * Json.t) list

let metadata_to_sexp md =
  Sexp.List
    (List.map
       ~f:(fun (k, v) -> [%sexp (k : string), (json_to_sexp v : Sexp.t)])
       md)

(* Currently unused *)
module type Cell_type = sig
  type cell

  val cell_of_json : Json.t -> cell
  val to_sexp : cell -> Sexp.t
end

module Markdown = struct
  type cell = { meta : metadata; attachments : metadata; source : Omd.doc }

  let to_sexp c =
    [%sexp
      "markdown",
        {
          meta = (metadata_to_sexp c.meta : Sexp.t);
          attachments = (metadata_to_sexp c.attachments : Sexp.t);
          source =
            (Parsexp.Single.parse_string_exn @@ Omd.to_sexp c.source : Sexp.t);
        }]

  open Omd

  let rec replace_markdown_attachments replacements : doc -> doc =
    List.map ~f:(replace_block_attachments replacements)

  and replace_block_attachments replacements : 'a block -> 'a block =
    let repl_inl inl = replace_inline_attachments replacements inl in
    let repl_md md = replace_markdown_attachments replacements md in
    function
    | Paragraph (attr, inl) -> Paragraph (attr, repl_inl inl)
    | List (attr, lt, ls, doc) -> List (attr, lt, ls, List.map ~f:repl_md doc)
    | Blockquote (attr, doc) -> Blockquote (attr, repl_md doc)
    | Heading (attr, lvl, inl) -> Heading (attr, lvl, repl_inl inl)
    | Definition_list (attr, defelts) ->
        Definition_list
          ( attr,
            List.map
              ~f:(fun { term; defs } ->
                { term = repl_inl term; defs = List.map ~f:repl_inl defs })
              defelts )
    | Table (attr, heads, lines) ->
        Table
          ( attr,
            List.map ~f:(fun (inl, ca) -> (repl_inl inl, ca)) heads,
            List.map ~f:(fun line -> List.map ~f:repl_inl line) lines )
    | x -> x

  and replace_inline_attachments replacements =
    let repl_inl inl = replace_inline_attachments replacements inl in
    function
    | Concat (attr, inls) -> Concat (attr, List.map ~f:repl_inl inls)
    | Emph (attr, inl) -> Emph (attr, repl_inl inl)
    | Strong (attr, inl) -> Strong (attr, repl_inl inl)
    | Image (attr, { label; destination; title }) as img ->
        let attachment = "attachment:" in
        if String.is_prefix destination ~prefix:attachment then
          let filename_old =
            String.chop_prefix_if_exists ~prefix:attachment destination
          in
          let filename_new =
            List.Assoc.find_exn ~equal:String.equal replacements filename_old
          in
          Image (attr, { label; destination = filename_new; title })
        else img
    | x -> x

  let make_attachments_unique c =
    let attachment_names = List.map ~f:(fun (a, _) -> a) c.attachments in
    let transfer_suffix fn newname =
      let suffix = List.last_exn (String.split fn ~on:'.') in
      String.concat [ newname; "."; suffix ]
    in
    let replacements =
      List.map
        ~f:(fun a -> (a, transfer_suffix a (Util.random_string ())))
        attachment_names
    in
    let attachments_new =
      List.map
        ~f:(fun ((old, nw), (old_, data)) -> (nw, data))
        (match List.zip replacements c.attachments with
        | Ok l -> l
        | _ -> assert false)
    in
    let md_new = replace_markdown_attachments replacements c.source in
    { c with source = md_new; attachments = attachments_new }

  let cell_of_json j =
    let al = cast_assoc j in
    let source =
      String.concat_lines
      @@ cast_string_list (find_assoc ~default:(`String "") al "source")
    in
    let doc = Omd.of_string source in
    let cell_type = cast_string @@ find_assoc_exn al "cell_type" in
    if not (String.equal cell_type "markdown") then
      raise
        (File_format_error
           [%sexp
             "Markdown.cell_of_json only handles markdown cells but got",
               (cell_type : string)]);
    make_attachments_unique
      {
        meta = cast_assoc (find_assoc ~default:(`Assoc []) al "metadata");
        attachments =
          cast_assoc (find_assoc ~default:(`Assoc []) al "attachments");
        source = doc;
      }
end

module Code = struct
  type output =
    | Stream of { name : string; text : string }
    | DisplayData of { data : (string * Json.t) list; meta : metadata }
    | ExecuteResult of { data : (string * Json.t) list; meta : metadata }
    | ErrorOutput of {
        ename : string;
        evalue : string;
        traceback : string list;
      }

  type cell = {
    execount : int;
    meta : metadata;
    source : string;
    outputs : output list;
  }

  let output_to_sexp = function
    | Stream { name; text } ->
        [%sexp "stream", { name = (name : string); text = (text : string) }]
    | DisplayData { data; meta } ->
        [%sexp
          "display_data",
            {
              data = (metadata_to_sexp data : Sexp.t);
              meta = (metadata_to_sexp meta : Sexp.t);
            }]
    | ExecuteResult { data; meta } ->
        [%sexp
          "execute_result",
            {
              data = (metadata_to_sexp data : Sexp.t);
              meta = (metadata_to_sexp meta : Sexp.t);
            }]
    | ErrorOutput { ename; evalue; traceback } ->
        [%sexp
          "error",
            {
              ename = (ename : string);
              evalue = (evalue : string);
              traceback =
                (Sexp.List (List.map ~f:String.sexp_of_t traceback) : Sexp.t);
            }]

  let to_sexp c =
    let outputs = Sexp.List (List.map ~f:output_to_sexp c.outputs) in
    [%sexp
      "code",
        {
          execount = (c.execount : int);
          meta = (metadata_to_sexp c.meta : Sexp.t);
          source = (c.source : string);
          outputs = (outputs : Sexp.t);
        }]

  let parse_output output =
    let output = cast_assoc output in
    let get = find_assoc_exn output in
    match cast_string (get "output_type") with
    | "error" ->
        ErrorOutput
          {
            ename = cast_string (get "ename");
            evalue = cast_string (get "evalue");
            traceback = cast_string_list (get "traceback");
          }
    | "execute_result" ->
        ExecuteResult
          { data = cast_assoc (get "data"); meta = cast_assoc (get "metadata") }
    | "display_data" ->
        DisplayData
          { data = cast_assoc (get "data"); meta = cast_assoc (get "metadata") }
    | "stream" ->
        Stream
          { name = cast_string (get "name"); text = cast_string (get "text") }
    | x ->
        raise (File_format_error [%sexp "Unknown output type:", (x : string)])

  let output_type = function
    | ErrorOutput _ -> "error"
    | ExecuteResult _ -> "execute_result"
    | Stream _ -> "stream"
    | DisplayData _ -> "display_data"

  let parse_outputs = List.map ~f:parse_output

  let cell_of_json j =
    let al = cast_assoc j in
    let get = find_assoc_exn al in
    let cell_type = cast_string @@ find_assoc_exn al "cell_type" in
    if not (String.equal cell_type "code") then
      raise
        (File_format_error
           [%sexp
             "Code.cell_of_json only handles code cells but got",
               (cell_type : string)]);
    {
      execount = cast_int (get "execution_count");
      meta = cast_assoc (get "metadata");
      source = String.concat @@ cast_string_list (get "source");
      outputs = parse_outputs (cast_list (get "outputs"));
    }
end

module Raw = struct
  type cell = { meta : metadata; mime : string; source : string }

  let to_sexp c =
    [%sexp
      {
        meta = (metadata_to_sexp c.meta : Sexp.t);
        mime = (c.mime : string);
        source = (c.source : string);
      }]

  let cell_of_json js =
    let al = cast_assoc js in
    let get = find_assoc_exn al in
    let cell_type = cast_string @@ find_assoc_exn al "cell_type" in
    if not (String.equal cell_type "raw") then
      raise
        (File_format_error
           [%sexp
             "Raw.cell_of_json only handles raw cells but got",
               (cell_type : string)]);
    let mime =
      cast_string @@ find_assoc_exn (cast_assoc (get "metadata")) "format"
    in
    {
      meta = cast_assoc (get "metadata");
      mime;
      source = String.concat_lines @@ cast_string_list (get "source");
    }
end

type cell = Markdown of Markdown.cell | Code of Code.cell | Raw of Raw.cell
type notebook = { meta : metadata; cells : cell list; nbformat : string }

let cell_of_json js =
  let ct = cast_string (find_assoc_exn (cast_assoc js) "cell_type") in
  match ct with
  | "markdown" -> Markdown (Markdown.cell_of_json js)
  | "code" -> Code (Code.cell_of_json js)
  | "raw" -> Raw (Raw.cell_of_json js)
  | _ -> raise (File_format_error [%sexp "Unknown cell type: ", (ct : string)])

let cell_to_sexp = function
  | Markdown md -> Markdown.to_sexp md
  | Code c -> Code.to_sexp c
  | Raw r -> Raw.to_sexp r

let to_sexp notebook =
  [%sexp
    {
      meta = (json_to_sexp (`Assoc notebook.meta) : Sexp.t);
      nbformat = (notebook.nbformat : string);
      cells = (Sexp.List (List.map ~f:cell_to_sexp notebook.cells) : Sexp.t);
    }]

let notebook_of_json js =
  let al = cast_assoc js in
  let get = find_assoc_exn al in
  let nbformat =
    String.concat
      [
        Int.to_string
          (cast_int @@ find_assoc ~default:(`String "") al "nbformat");
        ".";
        Int.to_string
          (cast_int @@ find_assoc ~default:(`String "") al "nbformat_minor");
      ]
  in
  {
    meta = cast_assoc (get "metadata");
    cells = List.map ~f:cell_of_json (cast_list (get "cells"));
    nbformat;
  }

let notebook_of_string s = notebook_of_json (Json.from_string s)
