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
