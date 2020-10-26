;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "ff660011c82c6af504915833e2d981a547b7ad58")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e38cf838c7b47b5f1353d00901b939ffa36d707")))

(when (featurep! +lsp)
  (package! dap-mode :pin "b414ae649ea4f25876a24946e379581a1fa6fe31")
  (package! posframe :pin "a99da9f40fa864910fd0234bb9e1b6fa52e699c3"))
