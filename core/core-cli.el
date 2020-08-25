;;; core/core-cli.el --- -*- lexical-binding: t; no-byte-compile: t; -*-

(load! "autoload/process")
(load! "autoload/plist")
(load! "autoload/files")
;; (load! "autoload/output")
(require 'seq)

;; Ensure straight and the bare minimum is ready to go
(require 'core-vars)
(eval-and-compile (require 'core-modules))
(require 'core-packages)
(doom-initialize-core-packages)
(eval-and-compile (require 'core-cli-lib))

;; Don't generate superfluous files when writing temp buffers
(setq make-backup-files nil)

;;
;;; Variables

(defvar doom-auto-accept (getenv "YES")
  "If non-nil, Doom will auto-accept any confirmation prompts during batch
commands like `doom-cli-packages-install', `doom-cli-packages-update' and
`doom-packages-autoremove'.")

(defvar doom-auto-discard (getenv "FORCE")
  "If non-nil, discard all local changes while updating.")

(defvar doom-cli-file "cli"
  "The basename of CLI config files for modules.

These are loaded when a Doom's CLI starts up. There users and modules can define
additional CLI commands, or reconfigure existing ones to better suit their
purpose.")

;;
;;; Debugger

(defun doom-cli--debugger (&rest args)
  (cl-incf num-nonmacro-input-events)
  (cl-destructuring-bind (error data backtrace)
      (list (caadr args)
            (cdadr args)
            (doom-cli--backtrace))
    (print! (error "There was an unexpected error"))
    (print-group!
     (print! "%s %s" (bold "Message:") (get error 'error-message))
     (print! "%s %s" (bold "Data:") (cons error data))
     (when (and (bound-and-true-p straight-process-buffer)
                (string-match-p (regexp-quote straight-process-buffer)
                                (get error 'error-message)))
       (print! (bold "Straight output:"))
       (let ((output (straight--process-get-output)))
         (appendq! data (list (cons "STRAIGHT" output)))
         (print-group! (print! "%s" output))))
     (when backtrace
       (print! (bold "Backtrace:"))
       (print-group!
        (dolist (frame (seq-take backtrace 10))
          (print! "%0.76s" frame)))
       (with-temp-file doom-cli-log-error-file
         (insert "# -*- lisp-interaction -*-\n")
         (insert "# vim: set ft=lisp:\n")
         (let ((standard-output (current-buffer))
               (print-quoted t)
               (print-escape-newlines t)
               (print-escape-control-characters t)
               (print-level nil)
               (print-circle nil))
           (mapc #'print (cons (list error data) backtrace)))
         (print! (warn "Extended backtrace logged to %s")
                 (relpath doom-cli-log-error-file))))))
  (throw 'exit 255))

(defun doom-cli--backtrace ()
  (let* ((n 0)
         (frame (backtrace-frame n))
         (frame-list nil)
         (in-program-stack nil))
    (while frame
      (when in-program-stack
        (push (cdr frame) frame-list))
      (when (eq (elt frame 1) 'doom-cli--debugger)
        (setq in-program-stack t))
      (when (and (eq (elt frame 1) 'doom-cli-execute)
                 (eq (elt frame 2) :doom))
        (setq in-program-stack nil))
      (setq n (1+ n)
            frame (backtrace-frame n)))
    (reverse frame-list)))


;;
;;; straight.el hacks

;; Straight was designed primarily for interactive use, in an interactive Emacs
;; session, but Doom does its package management in the terminal. Some things
;; must be modified get straight to behave and improve its UX for our users.

(defvar doom--straight-discard-options
  '(("has diverged from"
     . "^Reset [^ ]+ to branch")
    ("but recipe specifies a URL of"
     . "Delete remote \"[^\"]+\", re-create it with correct URL")
    ("has a merge conflict:"
     . "^Abort merge$")
    ("has a dirty worktree:"
     . "^Discard changes$")
    ("^In repository "
     . "^Reset branch \\|^Delete remote [^,]+, re-create it with correct URL"))
  "A list of regexps, mapped to regexps.

Their CAR is tested against the prompt, and CDR is tested against the presented
option, and is used by `straight-vc-git--popup-raw' to select which option to
recommend.

It may not be obvious to users what they should do for some straight prompts,
so Doom will recommend the one that reverts a package back to its (or target)
original state.")


;; HACK Remove dired & magit options from prompt, since they're inaccessible in
;;      noninteractive sessions.
(advice-add #'straight-vc-git--popup-raw :override #'straight--popup-raw)

;; HACK Replace GUI popup prompts (which hang indefinitely in tty Emacs) with
;;      simple prompts.
(defadvice! doom--straight-fallback-to-y-or-n-prompt-a (orig-fn &optional prompt)
  :around #'straight-are-you-sure
  (or doom-auto-accept
      (if doom-interactive-p
          (funcall orig-fn prompt)
        (y-or-n-p (format! "%s" (or prompt ""))))))

(defun doom--straight-recommended-option-p (prompt option)
  (cl-loop for (prompt-re . opt-re) in doom--straight-discard-options
           if (string-match-p prompt-re prompt)
           return (string-match-p opt-re option)))

(defadvice! doom--straight-fallback-to-tty-prompt-a (orig-fn prompt actions)
  "Modifies straight to prompt on the terminal when in noninteractive sessions."
  :around #'straight--popup-raw
  (if doom-interactive-p
      (funcall orig-fn prompt actions)
    (let ((doom--straight-discard-options doom--straight-discard-options))
      ;; We can't intercept C-g, so no point displaying any options for this key
      ;; when C-c is the proper way to abort batch Emacs.
      (delq! "C-g" actions #'assoc)
      ;; HACK These are associated with opening dired or magit, which isn't
      ;;      possible in tty Emacs, so...
      (delq! "e" actions #'assoc)
      (delq! "g" actions #'assoc)
      (if doom-auto-discard
          (cl-loop with doom-auto-accept = t
                   for (_key desc func) in actions
                   when desc
                   when (doom--straight-recommended-option-p prompt desc)
                   return (funcall func))
        (print! (start "%s") (red prompt))
        (print-group!
         (terpri)
         (let (options)
           (print-group!
            (print! " 1) Abort")
            (cl-loop for (_key desc func) in actions
                     when desc
                     do (push func options)
                     and do
                     (print! "%2s) %s" (1+ (length options))
                             (if (doom--straight-recommended-option-p prompt desc)
                                 (progn
                                   (setq doom--straight-discard-options nil)
                                   (green (concat desc " (Recommended)")))
                               desc))))
           (terpri)
           (let* ((options
                   (cons (lambda ()
                           (let ((doom-output-indent 0))
                             (terpri)
                             (print! (warn "Aborted")))
                           (kill-emacs 1))
                         (nreverse options)))
                  (prompt
                   (format! "How to proceed? (%s) "
                            (mapconcat #'number-to-string
                                       (number-sequence 1 (length options))
                                       ", ")))
                  answer fn)
             (while (null (nth (setq answer (1- (read-number prompt)))
                               options))
               (print! (warn "%s is not a valid answer, try again.")
                       answer))
             (funcall (nth answer options)))))))))

(defadvice! doom--straight-respect-print-indent-a (args)
  "Indent straight progress messages to respect `doom-output-indent', so we
don't have to pass whitespace to `straight-use-package's fourth argument
everywhere we use it (and internally)."
  :filter-args #'straight-use-package
  (cl-destructuring-bind
      (melpa-style-recipe &optional no-clone no-build cause interactive)
      args
    (list melpa-style-recipe no-clone no-build
          (if (and (not cause)
                   (boundp 'doom-output-indent)
                   (> doom-output-indent 0))
              (make-string (1- (or doom-output-indent 1)) 32)
            cause)
          interactive)))


;;
;;; Entry point

(defcli! :doom
    ((help-p        ["-h" "--help"]  "Same as help command")
     (auto-accept-p ["-y" "--yes"]   "Auto-accept all confirmation prompts")
     (debug-p       ["-d" "--debug"] "Enables on verbose output")
     (doomdir       ["--doomdir"  dir] "Use the private module at DIR (e.g. ~/.doom.d)")
     (localdir      ["--localdir" dir] "Use DIR as your local storage directory")
     &optional command
     &rest args)
  "A command line interface for managing Doom Emacs.

Includes package management, diagnostics, unit tests, and byte-compilation.

This tool also makes it trivial to launch Emacs out of a different folder or
with a different private module."
  (condition-case e
      (with-output-to! doom-cli-log-file
        (catch 'exit
          (when (and (not (getenv "__DOOMRESTART"))
                     (or doomdir
                         localdir
                         debug-p
                         auto-accept-p))
            (when doomdir
              (setenv "DOOMDIR" (file-name-as-directory doomdir))
              (print! (info "DOOMDIR=%s") localdir))
            (when localdir
              (setenv "DOOMLOCALDIR" (file-name-as-directory localdir))
              (print! (info "DOOMLOCALDIR=%s") localdir))
            (when debug-p
              (setenv "DEBUG" "1")
              (print! (info "DEBUG=1")))
            (when auto-accept-p
              (setenv "YES" auto-accept-p)
              (print! (info "Confirmations auto-accept enabled")))
            (setenv "__DOOMRESTART" "1")
            (throw 'exit :restart))
          (when help-p
            (when command
              (push command args))
            (setq command "help"))
          (if (null command)
              (doom-cli-execute "help")
            (let ((start-time (current-time)))
              (run-hooks 'doom-cli-pre-hook)
              (when (apply #'doom-cli-execute command args)
                (run-hooks 'doom-cli-post-hook)
                (print! (success "Finished in %.4fs")
                        (float-time (time-subtract (current-time) start-time))))))))
    ;; TODO Not implemented yet
    (doom-cli-command-not-found-error
     (print! (error "Command 'doom %s' not recognized") (string-join (cdr e) " "))
     (print! "\nDid you mean one of these commands?")
     (apply #'doom-cli-execute "help" "--similar" (string-join (cdr e) " "))
     2)
    ;; TODO Not implemented yet
    (doom-cli-wrong-number-of-arguments-error
     (cl-destructuring-bind (route opt arg n d) (cdr e)
       (print! (error "doom %s: %S requires %d arguments, but %d given\n")
               (mapconcat #'symbol-name route " ") arg n d)
       (print-group!
        (apply #'doom-cli-execute "help" (mapcar #'symbol-name route))))
     3)
    ;; TODO Not implemented yet
    (doom-cli-unrecognized-option-error
     (let ((option (cadr e)))
       (print! (error "Unrecognized option: %S") option)
       (when (string-match "^--[^=]+=\\(.+\\)$" option)
         (print! "The %S syntax isn't supported. Use '%s %s' instead."
                 option (car (split-string option "="))
                 (match-string 1 option))))
     4)
    ;; TODO Not implemented yet
    (doom-cli-deprecated-error
     (cl-destructuring-bind (route . commands) (cdr e)
       (print! (warn "The 'doom %s' command was removed and replaced with:\n")
               (mapconcat #'symbol-name route " "))
       (print-group!
        (dolist (command commands)
          (print! (info "%s") command))))
     5)
    (user-error
     (print! (warn "%s") (cadr e))
     1)))


;;
;;; CLI Commands

(load! "cli/help")
(load! "cli/install")
(load! "cli/sync")
(load! "cli/env")
(load! "cli/upgrade")
(load! "cli/packages")
(load! "cli/autoloads")

(defcligroup! "Diagnostics"
  "For troubleshooting and diagnostics"
  (load! "cli/doctor")
  (load! "cli/debug")

  ;; Our tests are broken at the moment. Working on fixing them, but for now we
  ;; disable them:
  ;; (load! "cli/test")
  )

(defcligroup! "Compilation"
  "For compiling Doom and your config"
  (load! "cli/byte-compile"))

(defcligroup! "Utilities"
  "Conveniences for interacting with Doom externally"
  (defcli! run (&rest args)
    "Run Doom Emacs from bin/doom's parent directory.

All arguments are passed on to Emacs.

  doom run
  doom run -nw init.el

WARNING: this command exists for convenience and testing. Doom will suffer
additional overhead by being started this way. For the best performance, it is
best to run Doom out of ~/.emacs.d and ~/.doom.d."
    (throw 'exit (cons invocation-name args))))


;;
;;; Bootstrap

(doom-log "Initializing Doom CLI")

;; Use our own home-grown debugger to display and log errors + backtraces.
;; Control over its formatting is important, because Emacs produces
;; difficult-to-read debug information otherwise. By making its errors more
;; presentable (and storing them somewhere users can access them later) we go a
;; long way toward making it easier for users to write better bug reports.
(setq debugger #'doom-cli--debugger
      debug-on-error t
      debug-ignored-errors nil)

;; Clean slate for the next invocation
(delete-file doom-cli-log-file)
(delete-file doom-cli-log-error-file)

;; Create all our core directories to quell file errors
(mapc (doom-rpartial #'make-directory 'parents)
      (list doom-local-dir
            doom-etc-dir
            doom-cache-dir))

(load! doom-module-init-file doom-private-dir t)
(maphash (doom-module-loader doom-cli-file) doom-modules)
(load! doom-cli-file doom-private-dir t)

(provide 'core-cli)
;;; core-cli.el ends here
