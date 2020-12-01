;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; lang/scheme/packages.el

(package! geiser :pin "2accab72e289ed82707237d2013ba034c88ff6c2")

(when (featurep! :checkers syntax)
  (package! flycheck-guile
    :recipe (:host github :repo "flatwhatson/flycheck-guile")
    :pin "27f4be2eaadabd4a51060fd4600e76be1e57bc7a"))
