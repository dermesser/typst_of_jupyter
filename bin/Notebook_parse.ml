module Json_util = Json_util
open Json_util
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

  (* TODO: "uniquify" attachment filenames here. *)

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
      source = String.concat_lines @@ cast_string_list (get "source");
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
