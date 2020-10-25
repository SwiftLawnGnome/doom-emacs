;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (progn
      (package! eglot :pin "61b71ea769fa14887465517f70832861f7052816")
      (package! project :pin "2965f90868bdd8ad7696fa22a764b510e133b67a"))
  (package! lsp-mode :pin "b76b85eca251e4573f03b4f69313e96aec628aa9")
  (package! lsp-ui :pin "25552041f5af110c282fe8a2c714dec0f7a2320e")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "20cac6296e5038b7131ee6f34a96635f1d30fe3c"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "fc09aa0903ee6abe4955e9a6062dcea667ebff5a")))
