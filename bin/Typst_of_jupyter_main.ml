
module Json = Yojson.Basic

module Render = Typst_of_notebook.Render

let filename = "Test Notebook.ipynb"
let () =
  let nb = In_channel.with_open_text filename (fun ic -> Notebook.notebook_of_string (In_channel.input_all ic)) in
  let fn = Typst_of_notebook.nb_to_typst ~asset_path:"typstofjupyter_assets" nb in
  Printf.printf "Wrote typst file at %s\n" fn
