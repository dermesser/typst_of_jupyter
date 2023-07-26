
module Json = Yojson.Basic

let filename = "Test Notebook.ipynb"
let () =
  let nb = In_channel.with_open_text filename (fun ic -> Notebook_parse.notebook_of_string (In_channel.input_all ic)) in
  let typst = Typst_of_notebook.nb_to_typst nb in
  Out_channel.output_string Out_channel.stdout typst
