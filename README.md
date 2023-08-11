# `typst_of_jupyter`

This project aims to convert Jupyter notebooks to PDFs by first generating typst (see [typst.app](https://typst.app))
source code. This has the potential of generating better PDFs than the existing LaTeX route, or through HTML and a
browser.

The goal is achieved by treating markdown and code cells and their results to specifically achieve the best typst code
for each kind. Features such as the built-in source code highlighting of typst are used.

In addition, this approach makes it possible to provide your own style or change the default one.

It is written in OCaml, making it simple to distribute as a binary running mostly anywhere. Binaries will be provided
soon through Github.

## Usage

(to be added - currently, even the file name is hardcoded)

The output is stored in a new or existing directory `typstofjupyter_assets`, containing both the typst source code and
all extracted image assets.

## Status

typst\_of\_jupyter works on basic notebooks (PNG/SVG results, markdown with attachments but no math, source code etc.).

The module converting markdown to typst aims to be reusable outside of this tool.

## To Do

* Enable basic usability, and add command line interface!!
* Conversion of math expressions from TeX (Mathjax) to typst
* Proper scaling of images with weird sizes
* Improved styling possibilities, with option to supply either your own configuration or your own template.
* Directly invoke the `typst` binary after finishing the output.

