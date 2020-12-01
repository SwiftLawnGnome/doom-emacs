;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  ;; REVIEW Broken in 8abf2c1f4f0ade64cbb06c8f47055f04ab83e8d6 (latest commit at
  ;;        time of writing). Revisit later.
  (package! iedit :pin "313997a2504e565a34e84fdb59a5a7ffd223328b")
  (package! evil-multiedit :pin "9f271e0e6048297692f80ed6c5ae8994ac523abc")
  (package! evil-mc :pin "7dfb2ca5ac00c249cb2f55cd6fa91fb2bfb1117e"))

 ((package! multiple-cursors :pin "83abb0533a9d9635bc86d6d52a4e7641b0eaaf63")))
