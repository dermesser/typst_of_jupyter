
module Json = Yojson.Basic

module Render = Typst_of_notebook.Render

let filename = "Test Notebook.ipynb"
let () =
  let nb = In_channel.with_open_text filename (fun ic -> Notebook_parse.notebook_of_string (In_channel.input_all ic)) in
  let text, render = Typst_of_notebook.nb_to_typst nb in
  let fn = Typst_of_notebook.write_render render text in
  Printf.printf "Wrote typst file at %s\n" fn
