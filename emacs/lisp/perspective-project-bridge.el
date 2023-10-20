;;; perspective-project-bridge.el --- Integration of perspective.el + project.el -*- lexical-binding: t -*-

;;
;; Author: Arunkumar Vaidyanathan <arunkumarmv1997@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (perspective "2.18"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Keywords: vc, perspective, project, project.el

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Creates a perspective for each project.el project.  (Based on the persp-mode-project-bridge)

;;; Usage:
;; Example configuration:

;; (with-eval-after-load "perspective-project-bridge-autoloads"
;;   (add-hook 'perspective-project-bridge-mode-hook
;;             (lambda ()
;;                 (if perspective-project-bridge-mode
;;                     (perspective-project-bridge-find-perspectives-for-all-buffers)
;;                   (perspective-project-bridge-kill-perspectives))))
;;   (add-hook 'after-init-hook
;;             (lambda ()
;;                 (perspective-project-bridge-mode 1))
;;             t))
;; 
;; With use-package:
;; (use-package perspective-project-bridge
;;   :hook
;;   (perspective-project-bridge-mode . (lambda ()
;;                                       (if perspective-project-bridge-mode
;;                                           (perspective-project-bridge-find-perspectives-for-all-buffers)
;;                                         (perspective-project-bridge-kill-perspectives))))
;;   (persp-mode . perspective-project-bridge-mode))

;;; Code:


(require 'perspective)
(require 'project)
(require 'cl-lib)

(declare-function project-root "project")

(defvar perspective-project-bridge-mode nil)

(defvar perspective-project-bridge-persp nil)

(defgroup perspective-project-bridge nil
  "Perspective project.el integration."
  :group 'persp
  :group 'project
  :prefix "perspective-project-bridge-")

(defun perspective-project-bridge-add-new-persp (name)
  "Create a new perspective NAME."
  (let ((persp (persp-new name)))
    (with-perspective (persp-name persp)
      (setq perspective-project-bridge-persp t))
    persp))

(defun perspective-project-bridge-find-perspective-for-buffer (b)
  "Find a perspective for buffer B."
  (when (buffer-live-p b)
    (with-current-buffer b
      (when (and perspective-project-bridge-mode
		 (buffer-name b) (project-current))
	(let ((persp (perspective-project-bridge-add-new-persp
		      (project-name (project-current)))))
	  (with-perspective (persp-name persp)
	    (persp-add-buffer b))
	  persp)))))

(defun perspective-project-bridge-hook-switch (&rest _args)
  "Switch to a perspective when hook is activated."
  (let* ((b (current-buffer))
	 (persp (perspective-project-bridge-find-perspective-for-buffer b)))
    (when persp
      (persp-switch (persp-name persp))
      (persp-switch-to-buffer b))))

(defun perspective-project-bridge-find-perspectives-for-all-buffers ()
  "Find perspectives for all buffers."
  (when perspective-project-bridge-mode
    (mapc #'perspective-project-bridge-find-perspective-for-buffer
          (buffer-list))))

(defun perspective-project-bridge-kill-perspectives ()
  "Kill all bridge perspectives."
  (mapc #'persp-kill
	(cl-delete-if-not
	 (lambda (p)
	   (with-perspective p
	     perspective-project-bridge-persp))
	 (persp-names))))

(defvar perspective-project-bridge-switch-hooks
  (list 'find-file-hook 'dired-mode-hook 'vc-dir-mode-hook 'eshell-mode-hook))

;;;###autoload
(define-minor-mode perspective-project-bridge-mode
  "`persp' and `project.el' integration.
Creates perspectives for project.el projects."
  :require 'perspective-project-bridge
  :group 'perspective-project-bridge
  :init-value nil
  :global t

  (if perspective-project-bridge-mode
      (if persp-mode
          (progn
            ;; Add hooks
            (add-hook 'persp-mode-hook
                      (lambda ()
                        (unless persp-mode
                          (perspective-project-bridge-mode -1))))
            (dolist (hook perspective-project-bridge-switch-hooks)
              (add-hook hook #'perspective-project-bridge-hook-switch))
	    (persp-make-variable-persp-local 'perspective-project-bridge-persp))
        (message "You can not enable perspective-project-bridge-mode \
unless persp is active.")
        (setq perspective-project-bridge-mode nil))
    ;; Remove hooks
    (dolist (hook perspective-project-bridge-switch-hooks)
      (remove-hook hook #'perspective-project-bridge-hook-switch))))

(provide 'perspective-project-bridge)

;;; perspective-project-bridge.el ends here
