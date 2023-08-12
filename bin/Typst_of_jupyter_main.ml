open! Core_unix
module Json = Yojson.Basic
module Render = Typst_of_notebook.Render

let main ~asset_path ~notebook_file =
  let nb =
    In_channel.with_open_text notebook_file (fun ic ->
        Notebook.notebook_of_string (In_channel.input_all ic))
  in
  let fn = Typst_of_notebook.nb_to_typst ~asset_path nb in
  Printf.printf "Wrote typst file at %s\n" fn

let args =
  let%map_open.Command notebook_file = anon ("notebook_file" %: string)
  and asset_path =
    flag "asset-path"
      (optional_with_default "typstofjupyter_assets" string)
      ~doc:"Output path for generated files"
  in
  fun () -> main ~notebook_file ~asset_path

let command =
  Command.basic ~summary:"Convert Jupyter notebooks to typst source code"
    ~readme:(fun () -> "")
    args

let () = Command_unix.run ~version:"0.1" ~build_info:"0000" command
