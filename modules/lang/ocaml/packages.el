;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "ccde45bbc292123ec20617f1af7f7e19f7481545")

(unless (featurep! +lsp)
  (package! merlin :pin "d0278ca384943373abb28ec15999d4d91b527fe9")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "9e26c0a2699b7076cebc04ece59fb354eb84c11c")

(when (featurep! :tools eval)
  (package! utop :pin "7bc5117d3449fc19f5c706a6decfdb2a30984507"))

(when (featurep! :editor format)
  (package! ocamlformat
    :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))
    :pin "04e659f63b0d0b81976814700dd3005274ec683c"))

(package! dune
  :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el"))
  :pin "bfe4dfe0c4441e91b955c9de20131f0a82c2626b")
