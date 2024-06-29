
(* Convert a notebook to a string of typst code and write it to main_file, with
   the assets (images etc.) in asset_path. A header can specify custom
   styling. *)
val nb_to_typst :
  ?asset_path:string ->
  header:string ->
  ?main_file:string ->
  Notebook.notebook ->
  string

