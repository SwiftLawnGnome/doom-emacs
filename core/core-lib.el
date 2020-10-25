;;; core-lib.el -*- lexical-binding: t; -*-

;;
;;; Helpers

(require 'macroexp)
(eval-when-compile
  (require 'pcase)
  (require 'inline))
(require 'core-vars)

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-unquote hooks)))
    (unless (listp hook-list) (setq hook-list (list hook-list)))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
         if (eq (car-safe hook) 'quote)
         collect (cadr hook)
         else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun doom--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list 'even (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (doom--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "doom--setq-%s-for-%s-h"
                                          var mode))))))


;;
;;; Public library

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defsubst doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free error-free))
  (if (listp exp) exp (list exp)))

(defun doom-keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun doom-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defmacro doom-log (format-string &rest args)
  "Log to *Messages* if `doom-debug-p' is on.
Does not interrupt the minibuffer if it is in use, but still logs to *Messages*.
Accepts the same arguments as `message'."
  `(when (bound-and-true-p doom-debug-p)
     (let ((inhibit-message (active-minibuffer-window)))
       (message
        ,(concat (propertize "DOOM " 'face 'font-lock-comment-face)
                 (when (bound-and-true-p doom--current-module)
                   (propertize
                    (format "[%s/%s] "
                            (doom-keyword-name (car doom--current-module))
                            (cdr doom--current-module))
                    'face 'warning))
                 format-string)
        ,@args))))

(defun doom-try-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (doom-log "Running doom hook: %s" hook)
  (condition-case e
      (funcall hook)
    ((debug error)
     (signal 'doom-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun doom-load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (null (file-exists-p file))
      (unless noerror
        (signal 'file-error (list "No envvar file exists" file)))
    (when-let
        (env
         (with-temp-buffer
           (save-excursion
             (setq-local coding-system-for-read 'utf-8)
             (insert "\0\n") ; to prevent off-by-one
             (insert-file-contents file))
           (save-match-data
             (when (re-search-forward "\0\n *\\([^#= \n]*\\)=" nil t)
               (split-string (buffer-substring (match-beginning 1) (point-max))
                             "\0\n"
                             'omit-nulls)))))
      (setq-default
       process-environment
       (nconc (nreverse env)
              (default-value 'process-environment))
       exec-path
       (nconc (split-string (getenv "PATH") path-separator t)
              (list exec-directory))
       shell-file-name
       (or (getenv "SHELL")
           (default-value 'shell-file-name)))
      env)))

(defun doom-list* (elem &rest elems)
  "Return a new list with ELEM and ELEMS as elements, consed onto the last arg.

(doom-list* w x y z) == (cons w (cons x (cons y z)))"
  (declare (pure t) (side-effect-free error-free)
           (compiler-macro (lambda (_)
                             `(backquote-list* ,elem ,@elems))))
  (cond
    ((not elems) elem)
    ((not (cdr elems)) (cons elem (car elems)))
    (t (let* ((len (length elems))
              (last2 (nthcdr (- len 2) elems)))
         (setcdr last2 (cadr last2)))
       (cons elem elems))))


;;
;;; Functional library

(defalias 'doom-partial #'apply-partially)

(defun doom-rpartial (fn &rest args)
  "Return a function that is a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (nconc pre-args args))))

(defun doom-remove (predicate list)
  (declare (pure t) (side-effect-free t))
  (let ((result nil))
    (while list
      (unless (funcall predicate (car list))
        (push (car list) result))
      (setq list (cdr list)))
    (nreverse result)))

(defun doom-keep (predicate list)
  (declare (pure t) (side-effect-free t))
  (let ((result nil))
    (while list
      (when (funcall predicate (car list))
        (push (car list) result))
      (setq list (cdr list)))
    (nreverse result)))

;;
;;; Sugars

(defun dir! ()
  "Returns the directory of the emacs lisp file this macro is called from."
  (when-let (path (file!))
    (directory-file-name (file-name-directory path))))

(defun file! ()
  "Return the emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
        (load-file-name)
        ((stringp (car-safe current-load-list))
         (car current-load-list))
        (buffer-file-name)
        ((error "Cannot get this file-path"))))

(defmacro letenv! (envvars &rest body)
  "Lexically bind ENVVARS in BODY, like `let' but for `process-environment'."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence process-environment)))
     ,@(mapcar (lambda (var-val)
                 `(setenv ,(car var-val) ,(cadr var-val)))
               envvars)
     ,@body))

(defmacro letf! (bindings &rest body)
  "Temporarily rebind function and macros in BODY.

BINDINGS is either a) a list of, or a single, `defun' or `defmacro'-ish form, or
b) a list of (PLACE VALUE) bindings as `cl-letf*' would accept.

TYPE is either `defun' or `defmacro'. NAME is the name of the function. If an
original definition for NAME exists, it can be accessed as a lexical variable by
the same name, for use with `funcall' or `apply'. ARGLIST and BODY are as in
`defun'.

\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent 1))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defmacro))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) (macroexpand body))
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq body (pcase type
                   (`defmacro `(cl-macrolet ((,@rest)) ,body))
                   (`defun `(cl-letf* ((,(car rest) (symbol-function #',(car rest)))
                                       ((symbol-function #',(car rest)) (lambda ,@(cdr rest))))
                              (ignore ,(car rest))
                              ,body))
                   (_ (when (eq (car-safe type) 'function)
                        (setq type (list 'symbol-function type)))
                      (list 'cl-letf (list (cons type rest)) body)))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and anything that
writes to `standard-output'."
  `(if (bound-and-true-p doom-debug-p)
       (progn ,@forms)
     ,(if doom-interactive-p
          `(let ((inhibit-message t)
                 (save-silently t))
             (prog1 ,(macroexp-progn forms) (message "")))
        `(letf! ((standard-output (lambda (&rest _)))
                 (defun message (&rest _))
                 (defun load (file &optional noerror nomessage nosuffix must-suffix)
                   (ignore nomessage)
                   (funcall load file noerror t nosuffix must-suffix))
                 (defun write-region (start end filename &optional append visit lockname mustbenew)
                   (funcall write-region start end filename append
                            (or visit 'no-message) lockname mustbenew)))
           ,@forms))))

(defmacro eval-if! (cond then &rest body)
  "Expands to THEN if COND is non-nil, to BODY otherwise.
COND is checked at compile/expansion time, allowing BODY to be omitted entirely
when the elisp is byte-compiled. Use this for forms that contain expensive
macros that could safely be removed at compile time."
  (declare (indent 2))
  (if (eval cond)
      then
    (macroexp-progn body)))

(defmacro eval-when! (cond &rest body)
  "Expands to BODY if CONDITION is non-nil at compile/expansion time.
See `eval-if!' for details on this macro's purpose."
  (declare (indent 1))
  (when (eval cond)
    (macroexp-progn body)))


;;; Closure factories
(defmacro fn! (arglist &rest body)
  "Expands to (cl-function (lambda ARGLIST BODY...))"
  (declare (indent 1) (doc-string 1))
  `(cl-function (lambda ,arglist ,@body)))

(defmacro cmd! (&rest body)
  "Expands to (lambda () (interactive) ,@body).
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional cmd-prefix-arg &rest args)
  "Expands to a closure that interactively calls COMMAND with ARGS.
A factory for quickly producing interactive, prefixed commands for keybinds or
aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _)
     (interactive "P")
     (let ((current-prefix-arg (or ,cmd-prefix-arg arg)))
       (,(if args
             'funcall-interactively
           'call-interactively)
         ,command ,@args))))

(defmacro cmds! (&rest branches)
  "Expands to a `menu-item' dispatcher for keybinds."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    `(general-predicate-dispatch ,fallback
       :docstring ,docstring
       ,@branches)))

;; For backwards compatibility
(defalias 'λ! 'cmd!)
(defalias 'λ!! 'cmd!!)
;; DEPRECATED These have been superseded by `cmd!' and `cmd!!'
(define-obsolete-function-alias 'lambda! 'cmd! "3.0.0")
(define-obsolete-function-alias 'lambda!! 'cmd!! "3.0.0")


;;; Mutation
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro setq! (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'.

This can be used as a drop-in replacement for `setq'. Particularly when you know
a variable has a custom setter (a :set property in its `defcustom' declaration).
This triggers setters. `setq' does not."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                              ',var ,val))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (if (not values)
      place
    (gv-letplace (getter setter) place
      (let ((res getter))
        (while values
          (setq res `(cl-adjoin ,(pop values) ,res :test #'equal)))
        (funcall setter res)))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

;;; Loading
(defmacro add-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file.
The current file is the file from which `add-to-load-path!' is used."
  (let ((s (gensym "dir")))
    `(let ((default-directory ,(dir!))
           file-name-handler-alist)
       ,@(mapcar (lambda (dir)
                   `(let ((,s (expand-file-name ,dir)))
                      (unless (member ,s load-path) (push ,s load-path))))
                 (delete-dups (copy-sequence dirs))))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.

This is a wrapper around `eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent 1) (debug t))
  ;; (if (symbolp package)
  ;;     (unless (memq package (bound-and-true-p doom-disabled-packages))
  ;;       (list (if (or (not (bound-and-true-p byte-compile-current-file))
  ;;                     (require package nil 'noerror))
  ;;                 'progn
  ;;               'with-no-warnings)
  ;;             (let ((body (macroexp-progn body)))
  ;;               `(if (featurep ',package)
  ;;                    ,body
  ;;                  ;; We intentionally avoid `with-eval-after-load' to prevent
  ;;                  ;; eager macro expansion from pulling (or failing to pull) in
  ;;                  ;; autoloaded macros/packages.
  ;;                  (eval-after-load ',package ',body)))))
  ;;   (let ((p (car package)))
  ;;     (cond ((not (keywordp p))
  ;;            `(after! (:and ,@package) ,@body))
  ;;           ((memq p '(:or :any))
  ;;            (macroexp-progn
  ;;             (cl-loop for next in (cdr package)
  ;;                      collect `(after! ,next ,@body))))
  ;;           ((memq p '(:and :all))
  ;;            (dolist (next (cdr package))
  ;;              (setq body `((after! ,next ,@body))))
  ;;            (car body)))))

  ;; NOTE doing it this convoluted way prevents large chunks of code being
  ;; duplicated in complex PACKAGE specs
  (let (_) ;; this is `dlet' in emacs 28
    (defvar doom--after-func)
    (letrec ((doom--after-func `(lambda () ,@body))
             (letbs nil)
             (recurse
              (lambda (package)
                (if (or (stringp package) (symbolp package))
                    (unless (memq package doom-disabled-packages)
                      `((eval-after-load ',package ,doom--after-func)))
                  (let ((doom--after-func doom--after-func)
                        (p (if (keywordp (car package)) (pop package) :and)))
                    (cond
                      ((not (cdr package)) (funcall recurse (car package)))
                      ((memq p '(:or :any))
                       (unless (symbolp doom--after-func)
                         (let ((s (gensym "thunk")))
                           (push (list s doom--after-func) letbs)
                           (setq doom--after-func s)))
                       (mapcan recurse package))
                      ((memq p '(:and :all))
                       (dolist (next (reverse (cdr package)))
                         (setq doom--after-func
                               `(lambda () ,@(funcall recurse next))))
                       (funcall recurse (car package))))))))
             (eb (macroexp-progn (funcall recurse package))))
      (macroexp-let* (nreverse letbs) eb))))

(defun doom--handle-load-error (e target path)
  (let* ((source (file-name-sans-extension target))
         (err nil)
         (dir nil))
    (cond
      ((not (featurep 'core))
       (setq err 'error)
       (setq dir (file-name-directory path)))
      ((file-in-directory-p source doom-core-dir)
       (setq err 'doom-error)
       (setq dir doom-core-dir))
      ((file-in-directory-p source doom-private-dir)
       (setq err 'doom-private-error)
       (setq dir doom-private-dir))
      (t (setq err 'doom-module-error)
         (setq dir doom-emacs-dir)))
    (signal err (list (file-relative-name (concat source ".el") dir) e))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (let* ((path (or path
                   (dir!)
                   (error "Could not detect path to look for '%s' in"
                          filename)))
         (file (if path
                  `(expand-file-name ,filename ,path)
                filename)))
    `(condition-case-unless-debug e
         (let (file-name-handler-alist)
           (load ,file ,noerror 'nomessage))
       (doom-error (signal (car e) (cdr e)))
       (error (doom--handle-load-error e ,file ,path)))))

(defmacro defer-until! (condition &rest body)
  "Run BODY when CONDITION is true (checks on `after-load-functions'). Meant to
serve as a predicated alternative to `after!'."
  (declare (indent defun) (debug t))
  `(if ,condition
       (progn ,@body)
     ,(let ((fn (intern (format "doom--delay-form-%s-h" (sxhash (cons condition body))))))
        `(progn
           (fset ',fn (lambda (&rest args)
                        (when ,(or condition t)
                          (remove-hook 'after-load-functions #',fn)
                          (unintern ',fn nil)
                          (ignore args)
                          ,@body)))
           (put ',fn 'permanent-local-hook t)
           (add-hook 'after-load-functions #',fn)))))

(defmacro bound! (things &rest body)
  "Evaluates body after checking that each of FNS is `fboundp'.
If any are not, a `void-function' error is signalled. This is to
avoid \"not known to be defined\" and \"may not be defined at
runtime\" compiler warnings."
  (declare (indent 1))
  (setq body (macroexp-progn body))
  ;; no point if not compiling
  (when (bound-and-true-p byte-compile-current-file)
    (let (bound-check error-cond)
      (dolist (thing (reverse things))
        (if (eq (car-safe thing) 'function)
            (setq bound-check 'fboundp
                  error-cond 'void-function
                  thing (cadr thing))
          (setq bound-check 'boundp
                error-cond 'void-variable))
        (setq body `(if (,bound-check ',thing)
                        ,body
                      (signal ',error-cond '(,thing)))))))
  body)

(defmacro defer-feature! (feature &rest fns)
  "Pretend FEATURE hasn't been loaded yet, until FEATURE-hook or FN runs.

Some packages (like `elisp-mode' and `lisp-mode') are loaded immediately at
startup, which will prematurely trigger `after!' (and `with-eval-after-load')
blocks. To get around this we make Emacs believe FEATURE hasn't been loaded yet,
then wait until FEATURE-hook (or MODE-hook, if FN is provided) is triggered to
reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "doom--defer-feature-%s-a" feature))))
    `(progn
       (delq! ',feature features)
       (defadvice! ,advice-fn (&rest _)
         :before ',fns
         ;; Some plugins (like yasnippet) will invoke a fn early to parse
         ;; code, which would prematurely trigger this. In those cases, well
         ;; behaved plugins will use `delay-mode-hooks', which we can check for:
         (unless delay-mode-hooks
           ;; ...Otherwise, announce to the world this package has been loaded,
           ;; so `after!' handlers can react.
           (provide ',feature)
           (dolist (fn ',fns)
             (advice-remove fn #',advice-fn)))))))


;;; Hooks
(defvar doom--transient-counter 0)
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn (intern (format "doom--transient-%d-h" (cl-incf doom--transient-counter)))))
    ;; `(let ((sym ,hook-or-function))
    ;;    (defun ,fn (&rest _)
    ;;      ,(format "Transient hook for %S" (doom-unquote hook-or-function))
    ;;      ,@forms
    ;;      (let ((sym ,hook-or-function))
    ;;        (cond ((functionp sym) (advice-remove sym #',fn))
    ;;              ((symbolp sym)   (remove-hook sym #',fn))))
    ;;      (unintern ',fn nil))
    ;;    (cond ((functionp sym)
    ;;           (advice-add ,hook-or-function ,(if append :after :before) #',fn))
    ;;          ((symbolp sym)
    ;;           (put ',fn 'permanent-local-hook t)
    ;;           (add-hook sym #',fn ,append))))
    ;; NOTE in the 99% case where HOOK-OR-FUNCTION is a constant, avoid let to
    ;; allow this to be a toplevel form
    (macroexp-let2 nil sym hook-or-function
      `(progn
         (defun ,fn (&rest _)
           ,(format "Transient hook for %S" (doom-unquote hook-or-function))
           ,@forms
           (cond ((functionp ,sym) (advice-remove ,sym #',fn))
                 ((symbolp ,sym)   (remove-hook ,sym #',fn)))
           (unintern ',fn nil))
         (cond
           ((functionp ,sym)
            (advice-add ,hook-or-function ,(if append :after :before) ',fn))
           ((symbolp ,sym)
            (put ',fn 'permanent-local-hook t)
            (add-hook ,sym ',fn ,append)))))))

(defmacro add-hook-trigger! (hook-var &rest targets)
  "TODO"
  `(let ((fn (intern (format "%s-h" ,hook-var))))
     (fset fn (lambda (&rest _)
                (run-hook-wrapped ,hook-var #'doom-try-run-hook)
                (set ,hook-var nil)))
     (put ,hook-var 'permanent-local t)
     (dolist (on (list ,@targets))
       (if (functionp on)
           (advice-add on :before fn)
         (add-hook on fn)))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (debug t)
           (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point)))))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'doom-unquote rest)
                     (doom-enlist (doom-unquote (car rest))))))

            ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,append-p ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (doom--setq-hook-fns hooks var-vals)
      collect `(defun ,fn (&rest _)
                 ,(format "%s = %s" var (pp-to-string val))
                 (setq-local ,var ,val))
      collect `(when (fboundp ',fn)         ;; satisfy compiler
                 (remove-hook ',hook #',fn) ; ensure set order
                 (add-hook ',hook #',fn)))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop
      for (_var _val hook fn)
      in (doom--setq-hook-fns hooks vars 'singles)
      collect `(remove-hook ',hook ',fn))))


;;; Definers
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) ',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.

This has the same signature as `defadvice!' an exists as an easy undefiner when
testing advice (when combined with `rotate-text').

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))


(provide 'core-lib)
;;; core-lib.el ends here
