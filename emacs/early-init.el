;;; early-init.el -*- lexical-binding: t; -*-
;;

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Straight handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)


;;; early-init.el ends here
