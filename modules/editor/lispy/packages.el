;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; editor/lispyville/packages.el

(package! lispy :pin "8d994a0ef2182dba732f2b81d2b247bc8717b3e2")
(when (featurep! :editor evil)
  (package! lispyville :pin "0f13f26cd6aa71f9fd852186ad4a00c4294661cd"))
