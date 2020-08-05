;;; app/twitter/autoload.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'core-modules))

(eval-and-compile
  (require 'avy))

(defvar twittering-initial-timeline-spec-string)
(defvar +twitter/ace-link)
(defvar avy-style)
(declare-function twittering-ensure-whole-of-status-is-visible "twittering-mode")
(declare-function twittering-goto-next-thing "twittering-mode")
(declare-function doom-fallback-buffer "buffers")
(declare-function twit "twittering-mode")
(declare-function twittering-kill-buffer "twittering-mode")
(declare-function doom-buffers-in-mode "buffers")
(declare-function twittering-rerender-timeline-all "twittering-mode")
(declare-function avy--process "avy")
(declare-function avy--style-fn "avy")

(defvar +twitter-workspace-name "*Twitter*"
  "The name to use for the twitter workspace.")



;;;###autoload
(defun +twitter-display-buffer-fn (buf)
  "A replacement display-buffer command for `twittering-pop-to-buffer-function'
that works with the feature/popup module."
  (let ((win (selected-window)))
    (display-buffer buf)
    ;; This is required because the new window generated by `pop-to-buffer'
    ;; may hide the region following the current position.
    (twittering-ensure-whole-of-status-is-visible win)))

;;;###autoload
(defun +twitter-buffer-p (buf)
  "Return non-nil if BUF is a `twittering-mode' buffer."
  (eq 'twittering-mode (buffer-local-value 'major-mode buf)))


;;
;; Commands

(defvar +twitter--old-wconf nil)
;;;###autoload
(defun =twitter (arg)
  "Opens a workspace dedicated to `twittering-mode'."
  (interactive "P")
  (condition-case nil
      (progn
        (if (and (not arg) (featurep! :ui workspaces))
            (+workspace/new +twitter-workspace-name)
          (setq +twitter--old-wconf (current-window-configuration))
          (delete-other-windows)
          (switch-to-buffer (doom-fallback-buffer)))
        (call-interactively #'twit)
        (unless (get-buffer (car twittering-initial-timeline-spec-string))
          (error "Failed to open twitter"))
        (switch-to-buffer (car twittering-initial-timeline-spec-string))
        (dolist (name (cdr twittering-initial-timeline-spec-string))
          (split-window-horizontally)
          (switch-to-buffer name))
        (balance-windows)
        (call-interactively #'+twitter/rerender-all))
    (error (+twitter/quit-all))))

;;;###autoload
(defun +twitter/quit ()
  "Close the current `twitter-mode' buffer."
  (interactive)
  (when (eq major-mode 'twittering-mode)
    (twittering-kill-buffer)
    (cond ((one-window-p) (+twitter/quit-all))
          ((featurep! :ui workspaces)
           (+workspace/close-window-or-workspace))
          ((delete-window)))))

;;;###autoload
(defun +twitter/quit-all ()
  "Close all open `twitter-mode' buffers and the associated workspace, if any."
  (interactive)
  (when (featurep! :ui workspaces)
    (+workspace/delete +twitter-workspace-name))
  (when +twitter--old-wconf
    (set-window-configuration +twitter--old-wconf)
    (setq +twitter--old-wconf nil))
  (dolist (buf (doom-buffers-in-mode 'twittering-mode (buffer-list) t))
    (twittering-kill-buffer buf)))

;;;###autoload
(defun +twitter/rerender-all ()
  "Rerender all `twittering-mode' buffers."
  (interactive)
  (dolist (buf (doom-buffers-in-mode 'twittering-mode (buffer-list) t))
    (with-current-buffer buf
      (twittering-rerender-timeline-all buf)
      (setq-local line-spacing 0.2)
      (goto-char (point-min)))))

;;;###autoload
(defun +twitter/ace-link ()
  "Open a visible link, username or hashtag in a `twittering-mode' buffer."
  (interactive)
  (require 'avy)
  ;; REVIEW Is this necessary anymore with `link-hint'
  (let ((pt (avy-with +twitter/ace-link
              (avy-process
               (+twitter--collect-links)
               (avy--style-fn avy-style)))))
    (when (number-or-marker-p pt)
      (goto-char pt)
      (let ((uri (get-text-property (point) 'uri)))
        (if uri (browse-url uri))))))

(defun +twitter--collect-links ()
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (while (and (< (point) end)
                  (ignore-errors (twittering-goto-next-thing) t))
        (push (point) points))
      (nreverse points))))
