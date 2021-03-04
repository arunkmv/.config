;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Maximized screen on start-up
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq
 doom-font (font-spec :family "Source Code Pro for Powerline" :size 17 :weight 'Regular)
 doom-theme 'doom-gruvbox
 default-directory "~"
 org-directory "~/org/"
 display-line-numbers-type 'relative
 vterm-shell "/bin/zsh")

;; Use default treemacs theme instead of doom-atom theme
(when (featurep! :ui treemacs)
  (remove-hook 'doom-load-theme-hook #'doom-themes-treemacs-config))

(map!
 :map vterm-mode-map
 :n "-" #'vterm-send-up
 :n "=" #'vterm-send-down)

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
