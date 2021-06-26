(setq default-directory "~")

(setq
 doom-font (font-spec :family "Source Code Pro for Powerline" :size 17 :weight 'Regular)
 doom-theme 'doom-gruvbox-mod
 display-line-numbers-type 'relative
 display-time-default-load-average nil)

(display-battery-mode 1)
(display-time-mode 1)

;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(toggle-frame-fullscreen)

(when (featurep! :ui treemacs)
  (remove-hook 'doom-load-theme-hook #'doom-themes-treemacs-config))

(map!
 :map dired-mode-map
 :n "-" (lambda () (interactive) (find-alternate-file "..")))

(setq projectile-track-known-projects-automatically nil)

(setq-hook! '+doom-dashboard-mode-hook hl-line-mode -1)
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(defvar fancy-splash-image-template
  (expand-file-name "emacs-e-template.svg" doom-private-dir)
  "Default template svg used for the splash image, with substitutions from ")

(defvar fancy-splash-sizes
  `((:height 300 :min-height 50 :padding (0 . 2))
    (:height 250 :min-height 42 :padding (2 . 4))
    (:height 200 :min-height 35 :padding (3 . 3))
    (:height 150 :min-height 28 :padding (3 . 3))
    (:height 100 :min-height 20 :padding (2 . 2))
    (:height 75  :min-height 15 :padding (2 . 1))
    (:height 50  :min-height 10 :padding (1 . 0))
    (:height 1   :min-height 0  :padding (0 . 0)))
  "list of plists with the following properties
  :height the height of the image
  :min-height minimum `frame-height' for image
  :padding `+doom-dashboard-banner-padding' (top . bottom) to apply
  :template non-default template file
  :file file to use instead of template")

(defvar fancy-splash-template-colours
  '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
  "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

(unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
  (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

(defun fancy-splash-filename (theme-name height)
  (expand-file-name (concat (file-name-as-directory "theme-splashes")
                            theme-name
                            "-" (number-to-string height) ".svg")
                    doom-cache-dir))

(defun fancy-splash-clear-cache ()
  "Delete all cached fancy splash images"
  (interactive)
  (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
  (message "Cache cleared!"))

(defun fancy-splash-generate-image (template height)
  "Read TEMPLATE and create an image if HEIGHT with colour substitutions as
   described by `fancy-splash-template-colours' for the current theme"
  (with-temp-buffer
    (insert-file-contents template)
    (re-search-forward "$height" nil t)
    (replace-match (number-to-string height) nil nil)
    (re-search-forward "$width" nil t)
    (replace-match (number-to-string height) nil nil)
    (dolist (substitution fancy-splash-template-colours)
      (goto-char (point-min))
      (while (re-search-forward (car substitution) nil t)
        (replace-match (doom-color (cdr substitution)) nil nil)))
    (write-region nil nil
                  (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

(defun fancy-splash-generate-images ()
  "Perform `fancy-splash-generate-image' in bulk"
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image (or (plist-get size :template)
                                       fancy-splash-image-template)
                                   (plist-get size :height)))))

(defun ensure-theme-splash-images-exist (&optional height)
  (unless (file-exists-p (fancy-splash-filename
                          (symbol-name doom-theme)
                          (or height
                              (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-images)))

(defun get-appropriate-splash ()
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))

(setq fancy-splash-last-size nil)
(setq fancy-splash-last-theme nil)
(defun set-appropriate-splash (&rest _)
  (let ((appropriate-image (get-appropriate-splash)))
    (unless (and (equal appropriate-image fancy-splash-last-size)
                 (equal doom-theme fancy-splash-last-theme)))
    (unless (plist-get appropriate-image :file)
      (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
    (setq fancy-splash-image
          (or (plist-get appropriate-image :file)
              (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
    (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
    (setq fancy-splash-last-size appropriate-image)
    (setq fancy-splash-last-theme doom-theme)
    (+doom-dashboard-reload)))

(add-hook 'window-size-change-functions #'set-appropriate-splash)
(add-hook 'doom-load-theme-hook #'set-appropriate-splash)

(setq
 doom-modeline-buffer-encoding nil
 doom-modeline-major-mode-icon t
 doom-modeline-modal-icon nil)

(custom-set-faces!
  `(doom-modeline-evil-emacs-state :background ,(doom-color 'violet) :foreground ,(doom-color 'base0))
  `(doom-modeline-evil-normal-state :background ,(doom-color 'green) :foreground ,(doom-color 'base0))
  `(doom-modeline-evil-insert-state :background ,(doom-color 'dark-blue) :foreground ,(doom-color 'fg))
  `(doom-modeline-evil-visual-state :background ,(doom-color 'orange) :foreground ,(doom-color 'base0))
  `(doom-modeline-evil-motion-state :background ,(doom-color 'red) :foreground ,(doom-color 'fg))
  `(doom-modeline-evil-operator-state :background ,(doom-color 'yellow) :foreground ,(doom-color 'base0))
  `(doom-modeline-evil-replace-state :background ,(doom-color 'magenta) :foreground ,(doom-color 'fg)))

(map! :leader
      :desc "Modeline" :n "tm"
      #'doom-modeline-mode)

(setq-default
 evil-emacs-state-tag          " E "
 evil-normal-state-tag         " N "
 evil-insert-state-tag         " I "
 evil-visual-char-tag          " V "
 evil-visual-line-tag          " VL "
 evil-visual-screen-line-tag   " VSL "
 evil-visual-block-tag         " VB "
 evil-motion-state-tag         " M "
 evil-operator-state-tag       " O "
 evil-replace-state-tag        " R ")

(add-hook 'lsp-ui-doc-mode-hook
            (lambda ()
              (when lsp-ui-doc-mode
                (remove-hook 'post-command-hook #'lsp-ui-doc--make-request t))))

(map!
 :map lsp-ui-doc-mode-map
 :n "gh" #'lsp-ui-doc-glance)

(setq lsp-log-io nil
      lsp-python-ms-extra-paths ["./src/python" "./configs"])

(after! python
  (setq conda-env-home-directory (expand-file-name "~/.conda"))
  (custom-set-variables
   '(conda-anaconda-home (getenv "CONDA_HOME"))))

(after! lsp-clients
  (set-lsp-priority! 'clangd 1))  ; ccls has priority 0

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))

(setq org-directory "~/Org/"
      org-startup-folded 'content)

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "PARKED(p)" "|" "DONE(d)" "KILLED(k)")))

  (defface +org-todo-todo '((t)) "Face for org todo keyword TODO")
  (defface +org-todo-inprogress '((t)) "Face for org todo keyword INPROGRESS")
  (defface +org-todo-parked '((t)) "Face for org todo keyword PARKED")
  (defface +org-todo-done '((t)) "Face for org todo keyword DONE")
  (defface +org-todo-killed '((t)) "Face for org todo keyword KILLED")

  (custom-set-faces!
    `(+org-todo-todo :foreground ,(doom-color 'yellow) :inherit (bold-italic org-todo))
    `(+org-todo-inprogress :foreground ,(doom-color 'violet) :inherit (bold-italic org-todo))
    `(+org-todo-parked :foreground ,(doom-color 'orange) :inherit (bold-italic org-todo))
    `(+org-todo-done :foreground ,(doom-color 'green) :inherit (bold-italic org-todo))
    `(+org-todo-killed :foreground ,(doom-color 'red) :inherit (bold-italic org-todo)))

  (setq org-todo-keyword-faces
        '(("TODO" . +org-todo-todo)
          ("INPROGRESS" . +org-todo-inprogress)
          ("PARKED" . +org-todo-parked)
          ("DONE" . +org-todo-done)
          ("KILLED" . +org-todo-killed))))

(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("■" "■" "■")))

(after! org
  (setq org-tags-column 60))

(add-hook! 'org-mode-hook 'org-zotxt-mode)
(after! org
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))

(defun org-zotxt-insert-current-selection ()
  "Insert reference link for the currently selected item in Zotero"
  (interactive)
  (org-zotxt-insert-reference-link 4))

(map! :map org-mode-map
      :localleader
      (:prefix ("z" . "zotero")
       :desc "Link to selected item" "i" #'org-zotxt-insert-current-selection
       :desc "Link to an item"       "I" #'org-zotxt-insert-reference-link
       :desc "Open link"             "a" #'org-zotxt-open-attachment))

(after! (org zotxt)
  (org-link-set-parameters "zotero"
                           :face `(:foreground ,(doom-color 'red)
                                   :weight bold
                                   :slant italic)))

(add-hook! 'org-mode-hook 'org-beamer-mode)
(require 'ox-latex)

(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

(setq vterm-shell "/bin/zsh")

(map!
 :map vterm-mode-map
 :n "-" #'vterm-send-up
 :n "=" #'vterm-send-down)

(map! :leader
      :desc "Edit .env" :n "fv"
      (lambda ()
          (interactive)
          (find-file-other-window (expand-file-name "~/.env")))
      :desc "Edit .zshrc" :n "fz"
      (lambda ()
          (interactive)
          (find-file-other-window (expand-file-name "../zsh/.zshrc" doom-private-dir))))
