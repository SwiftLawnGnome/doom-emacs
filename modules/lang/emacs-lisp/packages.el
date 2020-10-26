;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

;; Fontification plugins
(package! highlight-quoted :pin "24103478158cd19fbcfb4339a3f1fa1f054f1469")

;; Tools
(package! macrostep :pin "424e3734a1ee526a1bd7b5c3cd1d3ef19d184267")
(package! overseer :pin "02d49f582e80e36b4334c9187801c5ecfb027789")
(package! elisp-def :pin "da1f76391ac0d277e3c5758203e0150f6bae0beb")
(package! elisp-demos :pin "05047654fbd342cb2463ec8ea562e4bba53c7be2")
(when (featurep! :checkers syntax)
  (package! flycheck-cask :pin "4b2ede6362ded4a45678dfbef1876faa42edbd58"))

;; Libraries
(package! buttercup :pin "f6f93353405cda51ad2778ae9247f77265b7cbfb")
