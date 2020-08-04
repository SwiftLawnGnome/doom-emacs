;;; lang/scala/autoload.el -*- lexical-binding: t; -*-

(eval-when-compile (require 'core-lib))

;;;###autoload
(defun +scala-comment-indent-new-line-fn (&optional _)
  "Continue the commnt on the current line.

Meant to be used for `scala-mode's `comment-line-break-function'."
  (let* ((state (syntax-ppss))
         (comment-start-pos (ppss-comment-or-string-start state)))
    (save-match-data
      (cond ((and (integerp (ppss-comment-depth state))
                  ;; Ensure that we're inside a scaladoc comment
                  (string-match-p "^/\\*\\*?[^\\*]?"
                                  (buffer-substring-no-properties
                                   comment-start-pos
                                   (min (+ comment-start-pos 4)
                                        (point-max))))
                  (let ((prev-line
                         (buffer-substring-no-properties
                          (line-beginning-position 0) (line-end-position 0))))
                    (or (string-match "^\\s-*\\*" prev-line)
                        (string-match "\\s-*/*" prev-line))))
             (newline nil t)
             (indent-according-to-mode)
             (insert (make-string (max 0 (- (1- (match-end 0))
                                            (match-beginning 0)))
                                  ?\s)
                     "*")
             (scala-indent:indent-on-scaladoc-asterisk))
            ((ppss-comment-depth state) ; for line comments
             (call-interactively #'comment-indent-new-line))
            (t
             (newline nil t)
             (indent-according-to-mode))))))

;;;###autoload
(defun +scala/open-repl ()
  "Open a scala repl. Uses `run-scala' if in a sbt project."
  (interactive)
  (if (and (require 'sbt-mode nil t)
           (fboundp 'sbt:find-root)
           (sbt:find-root))
      (let ((buffer-name (sbt:buffer-name)))
        (unless (comint-check-proc buffer-name)
          (kill-buffer buffer-name))
        (bound! (#'run-scala) (run-scala))
        (get-buffer buffer-name))
    (let* ((buffer-name "*scala-repl")
           (buffer
            (if (comint-check-proc buffer-name)
                (get-buffer buffer-name)
              (make-comint-in-buffer "scala-repl" buffer-name "scala"))))
      (display-buffer buffer)
      buffer)))
