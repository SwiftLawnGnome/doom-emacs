;;; lang/haskell/+lsp.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'core-modules)
  (require 'use-package))

(declare-function lsp! "common")

(use-package! lsp-haskell
  :after lsp-mode
  :preface (add-hook 'haskell-mode-local-vars-hook #'lsp!)
  :config
  (when (featurep! +ghcide)
    (setq lsp-haskell-server-path "ghcide"
          lsp-haskell-server-args nil))
  ;; Does some strange indentation if it pastes in the snippet
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))
