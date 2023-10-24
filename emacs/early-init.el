;;; early-init.el -*- lexical-binding: t; -*-
;;

;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but will be reset later by `gcmh-mode'. Not resetting it later will
;;   cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Straight handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)


;;; early-init.el ends here
