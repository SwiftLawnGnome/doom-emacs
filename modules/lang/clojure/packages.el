;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "75c28897c7e91aa130c71c076aa2a6ce2e02da8f")
(package! cider :pin "8ee6dcc2cdaf6461539a2f0a2bb7721633a1b08c")
(package! clj-refactor :pin "6db85b37b57497b56d97d5e5512160e5db85f798")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "152df7ffa1ba3ea6dfcb238fabbf50e1e1a4dc97"))
