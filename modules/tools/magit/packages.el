;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "7828691d41e0d384fdb9ddf2efd854d2b2806748")
  (when (featurep! +forge)
    (package! forge :pin "aa994f799a043b8aa2595a58fce1f611d6e3c2b2"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "a0e5d1f3c7dfcb4f18c1b0d57f1746a4872df5c6")
  (package! github-review :pin "db723740e02348c0760407e532ad667ef89210ec")
  (when (featurep! :editor evil +everywhere)
    (package! evil-magit :pin "88dc26ce59dbf4acb4e2891c79c4bd329553ba56")))
