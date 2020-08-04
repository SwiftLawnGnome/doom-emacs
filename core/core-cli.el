;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'seq)

(load! "autoload/process")
(load! "autoload/plist")
(load! "autoload/files")

;; Create all our core directories to quell file errors
(make-directory doom-local-dir 'parents)
(make-directory doom-etc-dir 'parents)
(make-directory doom-cache-dir 'parents)

;; Ensure straight and the bare minimum is ready to go
(require 'core-vars)
(require 'core-modules)
(require 'core-packages)
(doom-initialize-core-packages)
(require 'core-cli-lib)

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
;;; CLI Commands

(load! "cli/help")
(load! "cli/install")

(defcli! (refresh re) ()
  "Deprecated for 'doom sync'"
  :hidden t
  (user-error "'doom refresh' has been replaced with 'doom sync'. Use that instead"))

(defcli! (sync s)
    ((inhibit-envvar-p ["-e"] "Don't regenerate the envvar file")
     (inhibit-elc-p    ["-c"] "Don't recompile config")
     (update-p         ["-u"] "Update installed packages after syncing")
     (prune-p          ["-p" "--prune"] "Purge orphaned package repos & regraft them"))
  "Synchronize your config with Doom Emacs.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `doom!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files.
  4. Update Doom outside of Doom (e.g. with git)

It will ensure that unneeded packages are removed, all needed packages are
installed, autoloads files are up-to-date and no byte-compiled files have gone
stale."
  (print! (start "Synchronizing your config with Doom Emacs..."))
  (print-group!
   (delete-file doom-autoload-file)
   (when (and (not inhibit-envvar-p)
              (file-exists-p doom-env-file))
     (doom-cli-reload-env-file 'force))
   (run-hooks 'doom-sync-pre-hook)
   (doom-cli-packages-install)
   (doom-cli-packages-build)
   (when update-p
     (doom-cli-packages-update))
   (doom-cli-packages-purge prune-p 'builds-p prune-p prune-p)
   (run-hooks 'doom-sync-post-hook)
   (when (doom-autoloads-reload)
     (print! (info "Restart Emacs or use 'M-x doom/reload' for changes to take effect")))
   t))

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
    (apply #'doom-cli-execute-after invocation-name args)
    nil))

(provide 'core-cli)
;;; core-cli.el ends here
