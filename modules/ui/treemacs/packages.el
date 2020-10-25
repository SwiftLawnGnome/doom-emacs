;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "3b62e40e5f891b648aaa890e423db00f68f984db")
;; These packages have no :pin because they're in the same repo
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (featurep! :tools magit)
  (package! treemacs-magit))
(when (featurep! :ui workspaces)
  (package! treemacs-persp))
