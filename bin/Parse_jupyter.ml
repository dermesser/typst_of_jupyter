open Json_util
module Json = Yojson.Basic
module Assoc = Base.List.Assoc

(* https://nbformat.readthedocs.io/en/latest/ *)

exception Json_error of string

type metadata = (string * Json.t) list

module Markdown = struct
  type cell = { meta : metadata; source : Omd.doc }

  let cell_of_json j =
    let al = cast_assoc j in
    let content = cast_string (find_assoc ~default:(`String "") al "source") in
    let doc = Omd.of_string content in
    {
      meta = cast_assoc (find_assoc ~default:(`Assoc []) al "metadata");
      source = doc;
    }
end

module Code = struct
  type output (* todo *)

  type cell = {
    execount : int;
    meta : metadata;
    source : string;
    ouputs : output list;
  }
end

type cell = Markdown of Markdown.cell | Code of Code.cell
