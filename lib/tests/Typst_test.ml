
let md_doc_1 = Omd.of_string {|
# Markdown example
## A paragraph
Hello World, this is a paragraph.

Some *highlighted* **words**.

## A code block
This is a code block:
```ocaml
Hello World
```

## A list
* A list
* item
* item 2

### A definition list!

Term 1
: That's the first term

Term 2
: The second term

## Images and links

![An image alt text](test.png)

[Linky link](https://borgac.net)

And this is it.

|}

let%expect_test _ =
  Out_channel.output_string Out_channel.stdout (Typst.markdown_to_typst md_doc_1);
  [%expect {|
    = Markdown example


    == A paragraph

    Hello World, this is a paragraph.
    Some _highlighted_ *words*.

    == A code block

    This is a code block:

    ```ocaml
    Hello World
    ```

    == A list

    * A list
    * item
    * item 2


    === A definition list!

    / Term 1: That's the first term
    / Term 2: The second term



    == Images and links

    #figure(image("test.png", width: 80%), caption: "An image alt text")
    #link("https://borgac.net")[Linky link]
    And this is it. |}]
  
