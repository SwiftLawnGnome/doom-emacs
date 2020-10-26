;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "7b121fce50430a44bd743583c87d7645bf157be2")
(package! scala-mode :pin "1d08e885b1489313666c7f15a3962432a4f757ee")

(when (featurep! +lsp)
  (package! lsp-metals :pin "e42c0b2448847f5de8ae73beae4dd695b560c4e0"))
