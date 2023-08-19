module Json = Yojson.Basic
module Render = Typst_of_notebook.Render

type cliargs = {
  notebook_file : string;
  asset_path : string;
  header_file : string option;
}

let default_typst_header =
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
            dx: -4.5em, dy: 12pt)
    #block(fill: bgcolor, outset: 5pt, radius: 3pt, width: 100%, stroke: stroke, content)
]
#let pngimage(path) = image(path, width: 80%)
|}

let read_typst_header = function
  | None -> default_typst_header
  | Some filename -> In_channel.with_open_text filename In_channel.input_all

let main args =
  let nb =
    In_channel.with_open_text args.notebook_file (fun ic ->
        Notebook.notebook_of_string (In_channel.input_all ic))
  in
  let header = read_typst_header args.header_file in
  let fn =
    Typst_of_notebook.nb_to_typst ~asset_path:args.asset_path ~header nb
  in
  Printf.printf "Wrote typst file at %s\n" fn

let args =
  let open Command.Let_syntax in
  let open Command.Param in
  let%map notebook_file = anon ("notebook_file" %: string)
  and asset_path =
    flag "asset-path"
      (optional_with_default "typstofjupyter_assets" string)
      ~doc:"Output path for generated files"
  and header_file =
    flag "header-file" (optional string)
      ~doc:"File containing typst header code with styles etc."
  in
  fun () -> main { notebook_file; asset_path; header_file }

let command =
  Command.basic ~summary:"Convert Jupyter notebooks to typst source code"
    ~readme:(fun () -> "")
    args

let () = Command_unix.run ~version:"0.1" ~build_info:"0000" command
