
let md_doc_1 = Omd.of_string {|
# Markdown example
## A paragraph
Hello World, this is a paragraph.

## A code block
This is a code block:
```ocaml
Hello World
```

## A list
* A list
* item
* item 2

And this is it.

|}

let%expect_test _ =
  Out_channel.output_string Out_channel.stdout (Typst.markdown_to_typst md_doc_1);
  [%expect {|
    = Markdown example


    == A paragraph

    Hello World, this is a paragraph.

    == A code block

    This is a code block:
    ```ocaml
    Hello World
    ```

    == A list

    *A list
    *item
    *item 2

    And this is it. |}]
  