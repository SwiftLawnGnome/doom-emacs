;;; core/core-cli-lib.el -*- lexical-binding: t; -*-

(require 'seq)
(require 'core-lib)
(require 'core-vars)
(require 'core-output)

(eval-when-compile
  (require 'cl-lib))

(defvar doom--cli-commands (make-hash-table :test 'equal))
(defvar doom--cli-groups (make-hash-table :test 'equal))
(defvar doom--cli-group nil)

(cl-defstruct
    (doom-cli
      (:constructor nil)
      (:constructor
          make-doom-cli
          (name &key desc aliases optlist arglist plist fn
                &aux (optlist
                      (cl-loop for (symbol options desc) in optlist
                         for ((_ . soptions) (_ . params))
                          = (seq-group-by #'stringp options)
                         collect
                          (make-doom-cli-option :symbol symbol
                                                :flags soptions
                                                :args params
                                                :desc desc))))))
  (name nil :read-only t)
  (desc "TODO")
  aliases
  optlist
  arglist
  plist
  (fn (lambda (_) (print! "But nobody came!"))))

(cl-defstruct doom-cli-option
  (symbol)
  (flags ())
  (args ())
  (desc "TODO"))

(defun doom--cli-get-option (cli flag)
  (cl-loop for opt in (doom-cli-optlist cli)
     if (member flag (doom-cli-option-flags opt))
     return opt))

(defun doom--cli-process (cli args)
  (let* ((args (copy-sequence args))
         (arglist (copy-sequence (doom-cli-arglist cli)))
         (expected
          (or (cl-position-if (doom-rpartial #'memq cl--lambda-list-keywords)
                              arglist)
              (length arglist)))
         (got 0)
         restvar
         rest
         alist)
    (catch 'done
      (while args
        (let ((arg (pop args)))
          (cond ((eq (car arglist) '&rest)
                 (setq restvar (cadr arglist)
                       rest (cons arg args))
                 (throw 'done t))

                ((string-match "^\\(--\\([a-zA-Z0-9][a-zA-Z0-9-_]*\\)\\)\\(?:=\\(.+\\)\\)?$" arg)
                 (let* ((fullflag (match-string 1 arg))
                        (opt (doom--cli-get-option cli fullflag)))
                   (unless opt
                     (user-error "Unrecognized switch %S" (concat "--" (match-string 2 arg))))
                   (setf (alist-get (doom-cli-option-symbol opt) alist)
                         (or (if (doom-cli-option-args opt)
                                 (or (match-string 3 arg)
                                     (pop args)
                                     (user-error "%S expected an argument, but got none"
                                                 fullflag))
                               (if (match-string 3 arg)
                                   (user-error "%S was not expecting an argument, but got %S"
                                               fullflag (match-string 3 arg))
                                 fullflag))))))

                ((string-match "^\\(-\\([a-zA-Z0-9]+\\)\\)$" arg)
                 (let ((fullflag (match-string 1 arg))
                       (flag     (match-string 2 arg)))
                   (dolist (switch (split-string flag "" t))
                     (if-let (opt (doom--cli-get-option cli (concat "-" switch)))
                         (setf (alist-get (doom-cli-option-symbol opt) alist)
                               (if (doom-cli-option-args opt)
                                   (or (pop args)
                                       (user-error "%S expected an argument, but got none"
                                                   fullflag))
                                 fullflag))
                       (user-error "Unrecognized switch %S" (concat "-" switch))))))

                (arglist
                 (cl-incf got)
                 (let ((spec (pop arglist)))
                   (when (eq spec '&optional)
                     (setq spec (pop arglist)))
                   (setf (alist-get spec alist) arg))
                 (when (null arglist)
                   (throw 'done t)))

                (t
                 (push arg args)
                 (throw 'done t))))))
    (when (< got expected)
      (error "Expected %d arguments, got %d" expected got))
    (when rest
      (setf (alist-get restvar alist) rest))
    alist))

(defun doom-cli-get (command)
  "Return a CLI object associated by COMMAND name (string)."
  (while (and command (not (doom-cli-p command)))
    (setq command (gethash (cond
                             ((symbolp command) command)
                             ((stringp command) (intern command))
                             (command))
                           doom--cli-commands)))
  command)

(defsubst doom-cli-internal-p (cli)
  "Return non-nil if CLI is an internal (non-public) command."
  (string-prefix-p ":" (doom-cli-name cli)))

(defun doom-cli-execute (command &optional args)
  "Execute COMMAND (string) with ARGS (list of strings).

Executes a cli defined with `defcli!' with the name or alias specified by
COMMAND, and passes ARGS to it."
  (if-let (cli (doom-cli-get command))
      (funcall (doom-cli-fn cli)
               (doom--cli-process cli (remq nil args)))
    (user-error "Couldn't find any %S command" command)))

(defun doom-cli--execute-after (lines)
  (let ((post-script (concat doom-local-dir ".doom.sh"))
        (coding-system-for-write 'utf-8)
        (coding-system-for-read  'utf-8))
    (with-temp-file post-script
      (insert "#!/usr/bin/env sh\n"
              (save-match-data
                (cl-loop for env in process-environment
                   if (string-match "^\\([a-zA-Z0-9_]+\\)=\\(.+\\)$" env)
                   concat (format "export %s=%s;\n"
                                  (match-string 1 env)
                                  (shell-quote-argument (match-string 2 env)))))
              (format "\nexport PATH=\"%s:$PATH\"\n" (concat doom-emacs-dir "bin/"))
              "\n[ -x \"$0\" ] && rm -f \"$0\"\n"
              (if (stringp lines)
                  lines
                (string-join
                 (if (listp (car-safe lines))
                     (cl-loop for line in (doom-enlist lines)
                        collect (mapconcat #'shell-quote-argument (remq nil line) " "))
                   (list (mapconcat #'shell-quote-argument (remq nil lines) " ")))
                 "\n"))
              "\n"))
    (set-file-modes post-script #o700)))

(defun doom-cli-execute-after (&rest args)
  "Execute shell command ARGS after this CLI session quits.

This is particularly useful when the capabilities of Emacs' batch terminal are
insufficient (like opening an instance of Emacs, or reloading Doom after a 'doom
upgrade')."
  (doom-cli--execute-after args))

(defun doom-cli-execute-lines-after (&rest lines)
  "TODO"
  (doom-cli--execute-after (string-join lines "\n")))

;;;###autoload
(defmacro defcli! (name speclist &optional docstring &rest body)
  "Defines a CLI command.

COMMAND is a symbol or a list of symbols representing the aliases for this
command. DOCSTRING is a string description; its first line should be short
(under 60 characters), as it will be used as a summary for 'doom help'.

SPECLIST is a specification for options and arguments, which can be a list
specification for an option/switch in the following format:

  (VAR [FLAGS... ARGS...] DESCRIPTION)

Otherwise, SPECLIST accepts the same argument specifiers as `defun'.

BODY will be run when this dispatcher is called."
  (declare (indent 2) (doc-string 3))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring "TODO"))
  (let ((names (doom-enlist name))
        (optlist (doom-keep #'listp speclist))
        (arglist (doom-remove #'listp speclist))
        (plist (cl-loop for (key val) on body by #'cddr
                  while (keywordp key)
                  collect key
                  collect val)))
    `(let ((name ',(car names))
           (aliases ',(cdr names))
           (plist ',plist))
       (when doom--cli-group
         (setq plist (plist-put plist :group doom--cli-group)))
       (puthash
        name
        (make-doom-cli (symbol-name name)
                       :desc ,docstring
                       :aliases (mapcar #'symbol-name aliases)
                       :arglist ',arglist
                       :optlist ',optlist
                       :plist plist
                       :fn
                       (lambda (--alist--)
                         (ignore --alist--)
                         (let ,(cl-loop for opt in speclist
                                  for optsym = (if (listp opt) (car opt) opt)
                                  unless (memq optsym cl--lambda-list-keywords)
                                  collect (list optsym `(cdr (assq ',optsym --alist--))))
                           ,@body)))
        doom--cli-commands)
       (when aliases
         (mapc (doom-rpartial #'puthash name doom--cli-commands)
               aliases)))))

(defmacro defcligroup! (name docstring &rest body)
  "Declare all enclosed cli commands are part of the NAME group."
  (declare (indent 1) (doc-string 2))
  `(let ((doom--cli-group ,name))
     (puthash doom--cli-group ,docstring doom--cli-groups)
     ,@body))

(provide 'core-cli-lib)
;;; core-cli-lib.el ends here
