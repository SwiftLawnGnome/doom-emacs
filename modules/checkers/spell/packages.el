;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; checkers/spell/packages.el

(if (not (featurep! +flyspell))
    (package! spell-fu :pin "a7db58747131dca2eee0e0757c3d254d391ddd1c")
  (package! flyspell-correct :pin "6d603a1dc51918f7f8aaf99dd5443f74a0afc794")
  (cond ((featurep! :completion ivy)
         (package! flyspell-correct-ivy))
        ((featurep! :completion helm)
         (package! flyspell-correct-helm))
        ((package! flyspell-correct-popup)))
  (package! flyspell-lazy :pin "d57382cf06462931cb354f5630469425fce56396"))
