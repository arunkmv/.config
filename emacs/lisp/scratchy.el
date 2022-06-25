;;; lisp/scratchy.el --- A persistant scratch buffer utility -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; A scratch buffer utility with default and project specific scratch
;; buffers that can persist across emacs sessions.
;;
;;; Code:

(require 'projectile)

(defvar scratchy-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `scratchy-dir'.")

(defvar scratchy-dir (concat user-emacs-directory "scratch")
  "Where to save persistent scratch buffers.")

(defvar scratchy-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.

Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar scratchy-buffers nil
  "A list of active scratch buffers.")

(defvar scratchy-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put 'scratchy-current-project 'permanent-local t)

(defvar scratchy-buffer-hook ()
  "The hooks to run after a scratch buffer is created.")

(defun scratchy-load-persistent-scratch-buffer (&optional project-name)
  "Load contents of a persisant scratch file in `scratchy-dir' into current buffer.

Creates `scratchy-dir' if non-existant."
  (setq-local scratchy-current-project
              (or project-name
                  scratchy-default-file))
  (when (not (file-exists-p scratchy-dir))
    (make-directory scratchy-dir t))
  (let ((smart-scratch-file
         (expand-file-name (concat scratchy-current-project ".el")
                           scratchy-dir)))
    (when (file-readable-p smart-scratch-file)
      (message "Reading %s" smart-scratch-file)
      (cl-destructuring-bind (content point mode)
          (with-temp-buffer
            (save-excursion (insert-file-contents smart-scratch-file))
            (read (current-buffer)))
        (erase-buffer)
        (funcall mode)
        (insert content)
        (goto-char point)
        t))))

;;;###autoload
(defun scratchy-get-scratch-buffer (&optional dont-restore-p mode directory project-name)
  "Return a scratchpad buffer in major MODE."
  (let* ((buffer-name (if project-name
                          (format "*scratchy (%s)*" project-name)
                        "*scratchy*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer
        (or buffer (get-buffer-create buffer-name))
      (setq default-directory directory)
      (setq-local so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless buffer
          (scratchy-load-persistent-scratch-buffer project-name)
          (when (and (eq major-mode 'fundamental-mode)
                     (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) scratchy-buffers)
      (add-hook 'kill-buffer-hook #'scratchy-persist-scratch-buffer-h nil 'local)
      (run-hooks 'scratchy-buffer-hook)
      (current-buffer))))

;;;###autoload
(defun scratchy-persist-scratch-buffer-h ()
  "Save the current scratch buffer to a persistant scratch file in `scratchy-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (point (point))
        (mode major-mode))
    (with-temp-file
        (expand-file-name (concat (or scratchy-current-project
                                      scratchy-default-file)
                                  ".el")
                          scratchy-dir)
      (prin1 (list content
                   point
                   mode)
             (current-buffer)))))

;;;###autoload
(defun scratchy-persist-scratch-buffers-h ()
  "Save all scratch buffers to `scratchy-dir'."
  (setq scratchy-buffers
        (cl-delete-if-not #'buffer-live-p scratchy-buffers))
  (dolist (buffer scratchy-buffers)
    (with-current-buffer buffer
      (scratchy-persist-scratch-buffer-h))))

;;;###autoload
(when (not noninteractive)
  (add-hook 'kill-emacs-hook #'scratchy-persist-scratch-buffers-h))


;;
;;; Commands

;;;###autoload
(defun scratchy-open-scratch-buffer (&optional arg project-p same-window-p)
  "Pop up a persistent scratch buffer.

If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
  current project."
  (interactive "P")
  (let (projectile-enable-caching
	(project-root (and project-p (projectile-project-root))))
    (funcall
     (if same-window-p
	 #'switch-to-buffer
       #'pop-to-buffer)
     (scratchy-get-scratch-buffer
      arg
      (cond ((eq scratchy-initial-major-mode t)
	     (unless (or buffer-read-only
			 (derived-mode-p 'special-mode)
			 (string-match-p "^ ?\\*" (buffer-name)))
	       major-mode))
	    ((null scratchy-initial-major-mode)
	     nil)
	    ((symbolp scratchy-initial-major-mode)
	     scratchy-initial-major-mode))
      default-directory
      (when project-root
	    (funcall projectile-project-name-function project-root))))
    (if (and project-p (not project-root))
      (message "Current buffer is not associated with any known projects! Opened the default scratch buffer."))))

;;;###autoload
(defun scratchy-switch-to-scratch-buffer (&optional arg project-p)
  "Open a persistent scratch buffer in the current window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (scratchy-open-scratch-buffer arg project-p 'same-window))

;;;###autoload
(defun scratchy-open-project-scratch-buffer (&optional arg)
  "Pop up the persistent project scratch buffer.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (scratchy-open-scratch-buffer arg 'project nil))

;;;###autoload
(defun scratchy-switch-to-project-scratch-buffer (&optional arg)
  "Open the persistent project scratch buffer in the current window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (scratchy-switch-to-scratch-buffer arg 'project))

;;;###autoload
(defun scratchy-revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*scratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (scratchy-load-persistent-scratch-buffer scratchy-current-project)
    (message "Reloaded scratch buffer")))

;;;###autoload
(defun scratchy-delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `scratchy-dir'.

If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory scratchy-dir t)
        (message "Cleared %S" (abbreviate-file-name scratchy-dir)))
    (make-directory scratchy-dir t)
    (let ((file (read-file-name "Delete scratch file > " scratchy-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))

(provide 'scratchy)
