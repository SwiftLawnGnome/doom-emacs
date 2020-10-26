;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "7939e0dd9edadfd9258f09e5cd7115a9657e09ba")
;; These packages have no :pin because they're in the same repo
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (featurep! :tools magit)
  (package! treemacs-magit))
(when (featurep! :ui workspaces)
  (package! treemacs-persp))
