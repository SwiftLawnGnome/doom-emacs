;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "f9a11fe2d1b491fde8b4f1815575de7c890d7b38")
      (package! project :pin "2965f90868bdd8ad7696fa22a764b510e133b67a"))
  (package! lsp-mode :pin "e96968b3341f5b72662a0ed22b0958e28b8f8fbc")
  (package! lsp-ui :pin "b1693d610c4d2c44305eba2719e8d4097fdcdcb8")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "20cac6296e5038b7131ee6f34a96635f1d30fe3c"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "fc09aa0903ee6abe4955e9a6062dcea667ebff5a")))
