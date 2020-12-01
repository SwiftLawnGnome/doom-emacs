;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "cd052dfef602fe79d8dfbcf9f06e6da74412218b")
(package! dired-git-info :pin "b47f2b0c3a6cb9b7a62a4ee2605a492e512d40a9")
(package! diff-hl :pin "f6244abebd23372f1fa0f8bc8a15b2587c090427")
(package! dired-rsync :pin "fe5988154c843c70d080ae8a822f27ed83983d01")
(when (featurep! +ranger)
  (package! ranger :pin "caf75f0060e503af078c7e5bb50d9aaa508e6f3e"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "fc2dfa1e9eb8bf1c402a675e7089638d702a27a5"))
(package! fd-dired :pin "313def22c88519e61279e858b4f53bb84a93da36")
