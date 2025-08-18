# Spy-lang
**Simple python** or **Small python** or **Soretonaku python**

# Install
(1) Need to install OCaml, menhir, ocamllex, ocamlopt, ocamlfind via opam
`$ opam install ocaml menhir ocamllex ocamlopt ocamlfind`

(2) Clone this repository
`$ git clone git@github.com:d-kmr/spy-lang.git`

(3) Make
`$ make`

# Usage and options
`$ spyc [-lex|-show] <inputfile>.spy` (this produces <inputfile>.py)
`$ python3 <inputfile>.py`

Options
-lex: showing tokens of the input file (debugging mode)
-show: showing the input spy code
