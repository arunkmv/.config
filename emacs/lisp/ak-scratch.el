;;; lisp/scratch.el -*- lexical-binding: t; -*-

(defvar ak/scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `ak/scratch-dir'.")

(defvar ak/etc-dir)

(defvar ak/scratch-dir (concat ak/etc-dir "scratch")
  "Where to save persistent scratch buffers.")

(defvar ak/scratch-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.

Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar ak/scratch-buffers nil
  "A list of active scratch buffers.")

(defvar ak/scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put 'ak/scratch-current-project 'permanent-local t)

(defvar ak/scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is created.")

(defun ak/load-persistent-scratch-buffer (project-name)
  (setq-local ak/scratch-current-project
              (or project-name
                  ak/scratch-default-file))
  (when (not (file-exists-p ak/scratch-dir))
    (make-directory ak/scratch-dir t))
  (let ((smart-scratch-file
         (expand-file-name (concat ak/scratch-current-project ".el")
                           ak/scratch-dir)))
    (make-directory ak/scratch-dir t)
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
(defun ak/scratch-buffer (&optional dont-restore-p mode directory project-name)
  "Return a scratchpad buffer in major MODE."
  (let* ((buffer-name (if project-name
                          (format "*scratch (%s)*" project-name)
                        "*scratch*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer
        (or buffer (get-buffer-create buffer-name))
      (setq default-directory directory)
      (setq-local so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless buffer
          (ak/load-persistent-scratch-buffer project-name)
          (when (and (eq major-mode 'fundamental-mode)
                     (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) ak/scratch-buffers)
      (add-hook 'kill-buffer-hook #'ak/persist-scratch-buffer-h nil 'local)
      (run-hooks 'ak/scratch-buffer-hook)
      (current-buffer))))

;;;###autoload
(defun ak/persist-scratch-buffer-h ()
  "Save the current buffer to `ak/scratch-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (point (point))
        (mode major-mode))
    (with-temp-file
        (expand-file-name (concat (or ak/scratch-current-project
                                      ak/scratch-default-file)
                                  ".el")
                          ak/scratch-dir)
      (prin1 (list content
                   point
                   mode)
             (current-buffer)))))

;;;###autoload
(defun ak/persist-scratch-buffers-h ()
  "Save all scratch buffers to `ak/scratch-dir'."
  (setq ak/scratch-buffers
        (cl-delete-if-not #'buffer-live-p ak/scratch-buffers))
  (dolist (buffer ak/scratch-buffers)
    (with-current-buffer buffer
      (ak/persist-scratch-buffer-h))))

;;;###autoload
(when (not noninteractive)
  (add-hook 'kill-emacs-hook #'ak/persist-scratch-buffers-h))


;;
;;; Commands

(defvar projectile-enable-caching)
;;;###autoload
(defun ak/open-scratch-buffer (&optional arg project-p same-window-p)
  "Pop up a persistent scratch buffer.

If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
  current project."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     (if same-window-p
	 #'switch-to-buffer
       #'pop-to-buffer)
     (ak/scratch-buffer
      arg
      (cond ((eq ak/scratch-initial-major-mode t)
	     (unless (or buffer-read-only
			 (derived-mode-p 'special-mode)
			 (string-match-p "^ ?\\*" (buffer-name)))
	       major-mode))
	    ((null ak/scratch-initial-major-mode)
	     nil)
	    ((symbolp ak/scratch-initial-major-mode)
	     ak/scratch-initial-major-mode))
      default-directory
      (when (and project-p (fboundp 'ak/project-name))
		 (ak/project-name))))))

;;;###autoload
(defun ak/switch-to-scratch-buffer (&optional arg project-p)
  "Like `ak/open-scratch-buffer', but switches to it in the current window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (ak/open-scratch-buffer arg project-p 'same-window))

;;;###autoload
(defun ak/open-project-scratch-buffer (&optional arg same-window-p)
  "Opens the (persistent) project scratch buffer in a popup.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (ak/open-scratch-buffer arg 'project same-window-p))

;;;###autoload
(defun ak/switch-to-project-scratch-buffer (&optional arg)
  "Like `ak/open-project-scratch-buffer', but switches to it in the current
window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (ak/open-project-scratch-buffer arg 'same-window))

;;;###autoload
(defun ak/revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*scratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (ak/load-persistent-scratch-buffer ak/scratch-current-project)
    (message "Reloaded scratch buffer")))

;;;###autoload
(defun ak/delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `ak/scratch-dir'.

If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory ak/scratch-dir t)
        (message "Cleared %S" (abbreviate-file-name ak/scratch-dir)))
    (make-directory ak/scratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " ak/scratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))

(provide 'ak-scratch)
