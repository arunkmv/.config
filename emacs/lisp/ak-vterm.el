(require 'vterm)
(require 'project)

(defcustom ak-vterm-consult-zsh-history-file "~/.config/zsh/.zsh_history"
  "File containing zsh history."
  :type 'string
  :group 'vterm)

(defcustom ak-vterm-consult-zsh-history-lines 10000
  "Number of lines to read form `ak-vterm-consult-zsh-history-file'."
  :type 'integer
  :group 'vterm)

(defvar ak-vterm-consult-history)

(defun ak-vterm-consult-get-zsh-history (n)
  "Read N lines from `ak-vterm-consult-zsh-history-file'.
     Also formats commands and removes duplicates."
  (with-temp-buffer
    (insert-file-contents ak-vterm-consult-zsh-history-file)
    (let* ((all-lines (split-string (buffer-string) "\n" t))
	   (nlines (length all-lines))
	   (lines (if (< n nlines)
		      (nthcdr (- nlines n) all-lines)
		    all-lines))
	   (sl-cmds))
      (dolist (line lines sl-cmds)
	(if (string-match-p "^:" line)
	    (let* ((split-cmd (cdr (split-string line ";")))
		   (formatted-cmd))
	      (if (cdr split-cmd)
		  (dolist (frag split-cmd formatted-cmd)
		    (concat formatted-cmd ";" frag))
		(setq formatted-cmd (car split-cmd)))
	      (push formatted-cmd sl-cmds))
	  (push (concat (pop sl-cmds) line) sl-cmds)))
      (delete-dups sl-cmds))))

(defun ak-vterm-consult-zsh-history ()
  "Send command string from zsh history to `vterm'."
    (interactive)
    (if (eq major-mode 'vterm-mode)
        (let* ((beg (vterm--get-prompt-point))
               (end (vterm--get-cursor-point))
               (input (buffer-substring-no-properties
                       beg
                       end))
	       (cmd (consult--read (ak-vterm-consult-get-zsh-history ak-vterm-consult-zsh-history-lines)
				   :require-match t
				   :prompt "Command: "
				   :history 'consult-spotify-history
				   :sort nil)))
	  (vterm-delete-region beg end)
	  (vterm-send-string cmd))))

(defun ak-vterm-configure-project-root-and-display (arg display-fn)
  "Sets the environment variable PROOT and displays a terminal using `display-fn`.
    If prefix ARG is non-nil, cd into `default-directory' instead of project root.
    Returns the vterm buffer."
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (let* ((project-root (or (project-root (project-current)) default-directory))
	 (default-directory
	  (if arg
	      default-directory
	    project-root)))
    (setenv "PROOT" project-root)
    (funcall display-fn)))

;;;###autoload
(defun ak-vterm-toggle (arg)
  "Toggles a terminal popup window at project root.
      If prefix ARG is non-nil, recreate vterm buffer in the default directory.
      Returns the vterm buffer."
  (interactive "P")
  (ak-vterm-configure-project-root-and-display
   arg
   (lambda()
     (let ((buffer-name
	    (format "*vterm-popup:%s*"
		    (if (bound-and-true-p persp-mode)
			(persp-current-name)
		      "main")))
	   confirm-kill-processes
	   current-prefix-arg)
       (when arg
	 (let ((buffer (get-buffer buffer-name))
	       (window (get-buffer-window buffer-name)))
	   (when (buffer-live-p buffer)
	     (kill-buffer buffer))
	   (when (window-live-p window)
	     (delete-window window))))
       (if-let (win (get-buffer-window buffer-name))
	   (delete-window win)
	 (let ((buffer (get-buffer-create buffer-name)))
	   (with-current-buffer buffer
	     (unless (eq major-mode 'vterm-mode)
	       (vterm-mode)))
	   (pop-to-buffer buffer)))
       (get-buffer buffer-name)))))

;;;###autoload
(defun ak-vterm-here (arg)
  "Open a terminal buffer in the current window at project root.
  If prefix ARG is non-nil, cd into `default-directory' instead of project root.
  Returns the vterm buffer."
  (interactive "P")
  (ak-vterm-configure-project-root-and-display
   arg
   (lambda()
     (let (display-buffer-alist)
       (vterm vterm-buffer-name)))))

;;;###autoload
(defun ak-vterm-dir (directory)
  "Open a terminal buffer in the current window at DIRECTORY.
  Returns the vterm buffer."
  (interactive "f")
  (let ((default-directory (file-name-directory directory)))
    (ak-vterm-configure-project-root-and-display
     t
     (lambda()
       (let (display-buffer-alist)
	 (vterm vterm-buffer-name))))))

(provide 'ak-vterm)
