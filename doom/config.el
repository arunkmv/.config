;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ============================ DOOM Emacs ============================

(setq
 doom-font (font-spec :family "Source Code Pro for Powerline" :size 17 :weight 'Regular)
 doom-theme 'doom-gruvbox
 default-directory "~"
 display-line-numbers-type 'relative
 +doom-dashboard-banner-file (expand-file-name "emacs_logo.png" doom-private-dir))

;; Maximized screen on start-up
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Use default treemacs theme instead of doom-atom theme
(when (featurep! :ui treemacs)
  (remove-hook 'doom-load-theme-hook #'doom-themes-treemacs-config))

;; ============================ Prog mode ============================

;; Python
(setq lsp-log-io t)
(setq lsp-python-ms-extra-paths ["./src/python" "./configs"])

(custom-set-variables
 '(conda-anaconda-home (getenv "CONDA_HOME")))
(setq conda-env-home-directory (expand-file-name "~/.conda"))

;; C/C++
(after! lsp-clients
  (set-lsp-priority! 'clangd 1))  ; ccls has priority 0

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))

;; ========================= Dired + Projectile =========================

;; Change dired default behaviour of creating new buffers
(map!
 :map dired-mode-map
 :n "-" (lambda () (interactive) (find-alternate-file "..")))

(setq projectile-track-known-projects-automatically nil)

;; ============================ Org Mode ============================

(setq org-directory "~/Org/")

;; Customize todo keywords

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "PARKED(p)" "|" "DONE(d)" "KILLED(k)")))

  (with-no-warnings
    (custom-declare-face '+org-todo-todo `((t (:foreground ,(doom-color 'yellow) :inherit (bold-italic org-todo)))) "")
    (custom-declare-face '+org-todo-inprogress `((t (:foreground ,(doom-color 'violet) :inherit (bold-italic org-todo)))) "")
    (custom-declare-face '+org-todo-parked `((t (:foreground ,(doom-color 'orange) :inherit (bold-italic org-todo)))) "")
    (custom-declare-face '+org-todo-done `((t (:foreground ,(doom-color 'green) :inherit (bold-italic org-todo)))) "")
    (custom-declare-face '+org-todo-killed `((t (:foreground ,(doom-color 'red) :inherit (bold-italic org-todo)))) ""))

  (setq org-todo-keyword-faces
        '(("TODO" . +org-todo-todo)
          ("INPROGRESS" . +org-todo-inprogress)
          ("PARKED" . +org-todo-parked)
          ("DONE" . +org-todo-done)
          ("KILLED" . +org-todo-killed)))

  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))

;; zotxt-emacs
(add-hook! 'org-mode-hook (lambda () (org-zotxt-mode 1)))

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

;; Fancy priority icons
(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("■" "■" "■")))

;; ============================ Term + Shell ============================

 (setq vterm-shell "/bin/zsh")

;; Browse shell history in vterm
(map!
 :map vterm-mode-map
 :n "-" #'vterm-send-up
 :n "=" #'vterm-send-down)

;; Custom function to edit zsh .env file
(defun edit-env ()
  "Edit the .env file"
  (interactive)
  (find-file-other-window (expand-file-name "~/.env")))

(map! :leader :desc "Edit .env" :n "fv" #'edit-env)
