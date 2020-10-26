;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; config/default/packages.el

(package! avy :pin "bbf1e7339eba06784dfe86643bb0fbddf5bb0342")
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! link-hint :pin "1462dfeb908f2e07ee80e745bb9cebe6762374e3")

(unless (featurep! :editor evil)
  (package! expand-region :pin "ea6b4cbb9985ddae532bd2faf9bb00570c9f2781"))
