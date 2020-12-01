;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; lang/rust/packages.el

(package! rustic :pin "7ad2c83922b1aa006aab05a209740ae294bd3907")
(unless (featurep! +lsp)
  (package! racer :pin "a0bdf778f01e8c4b8a92591447257422ac0b455b"))
