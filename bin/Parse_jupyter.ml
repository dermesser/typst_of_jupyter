open Json_util
open! Util
open Base
module Json = Yojson.Basic
module Assoc = Base.List.Assoc

(* https://nbformat.readthedocs.io/en/latest/ *)

exception Json_error of string
exception File_format_error of Sexp.t

type metadata = (string * Json.t) list

module type Cell_type = sig
  type cell

  val cell_of_json : Json.t -> cell
end

module Markdown = struct
  type cell = { meta : metadata; source : Omd.doc }

  let cell_of_json j =
    let al = cast_assoc j in
    let content = cast_string (find_assoc ~default:(`String "") al "source") in
    let doc = Omd.of_string content in
    let cell_type = cast_string @@ find_assoc_exn al "cell_type" in
    if not (String.equal cell_type "markdown") then
      raise
        (File_format_error
           [%sexp
             "Markdown.cell_of_json only handles markdown cells but got",
               (cell_type : string)]);
    {
      meta = cast_assoc (find_assoc ~default:(`Assoc []) al "metadata");
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
    if not (String.equal cell_type "markdown") then
      raise
        (File_format_error
           [%sexp
             "Code.cell_of_json only handles code cells but got",
               (cell_type : string)]);
    {
      execount = cast_int (get "execution_count");
      meta = cast_assoc (get "metadata");
      source = cast_string (get "source");
      outputs = parse_outputs (cast_list (get "outputs"));
    }
end

module Raw = struct
  type cell = { meta : metadata; mime : string; source : string }

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
      source = cast_string (get "source");
    }
end

type cell = Markdown of Markdown.cell | Code of Code.cell | Raw of Raw.cell
