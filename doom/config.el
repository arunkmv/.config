;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Maximized screen on start-up
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq
 doom-font (font-spec :family "Source Code Pro for Powerline" :size 17 :weight 'Regular)
 doom-theme 'doom-gruvbox
 default-directory "~"
 org-directory "~/Org/"
 display-line-numbers-type 'relative
 vterm-shell "/bin/zsh"
 +doom-dashboard-banner-file (expand-file-name "emacs_logo.png" doom-private-dir))

;; Use default treemacs theme instead of doom-atom theme
(when (featurep! :ui treemacs)
  (remove-hook 'doom-load-theme-hook #'doom-themes-treemacs-config))

;; Browse shell history in vterm
(map!
 :map vterm-mode-map
 :n "-" #'vterm-send-up
 :n "=" #'vterm-send-down)

;; Prog mode
(setq lsp-log-io t)
(setq lsp-python-ms-extra-paths ["./src/python" "./configs"])

(custom-set-variables
 '(conda-anaconda-home (getenv "CONDA_HOME")))
(setq conda-env-home-directory (expand-file-name "~/.conda"))

(after! lsp-clients
  (set-lsp-priority! 'clangd 1))  ; ccls has priority 0

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))

;; Change dired default behaviour of creating new buffers
(map!
 :map dired-mode-map
 :n "-" (lambda () (interactive) (find-alternate-file "..")))

;; Custom function to edit the .env file
(defun edit-env ()
  "Edit the .env file"
  (interactive)
  (find-file-other-window (expand-file-name "~/.env")))

(map! :leader :desc "Edit .env" :n "fv" #'edit-env)

(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
