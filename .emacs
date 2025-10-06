;;; .emacs --- My custom Emacs setup -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my personal Emacs configuration file.
;;
;; Notice that it's splitted among several files in order to make it easier to
;; understand and segregate their configuration.  Other than that, I have added
;; some short documentation to describe each package.

;;; Code:

;; ============================================
;; PACKAGE MANAGER
;;
;; Next-generation, purely functional package manager for the Emacs hacker.
;; Save and load version lockfiles that ensure 100% reprocibility for my Emacs
;; configuration. Package state is defined entirely by the init-file and
;; (optional) lockfile, with no extra persistent data floating around.
;; Repository: `https://github.com/radian-software/straight.el'
;;
;; Bootstrap script:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

(require 'straight)

(straight-use-package 'use-package)
;; https://github.com/radian-software/straight.el#how-do-i-update-melpa-et-al
(defalias 'straight-update-all-packages 'straight-pull-all)
;; https://github.com/radian-software/straight.el#how-do-i-uninstall-a-package
(defalias 'straight-clean-packages 'straight-remove-unused-repos)
(setq straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake))

;; ============================================
;; Load external configuration
(message "Loading external configuration")
(defvar gajo--local-dir
  "~/Desktop/codes/emacs-config/"
  "Base path for the src files.")

(load-file (concat gajo--local-dir "src/helpers.el"))
(load-file (concat gajo--local-dir "src/theme.el"))
(load-file (concat gajo--local-dir "src/global.el"))
(load-file (concat gajo--local-dir "src/unfill-paragraph.el"))
(load-file (concat gajo--local-dir "src/org-mode.el"))
(load-file (concat gajo--local-dir "src/markdown.el"))
(load-file (concat gajo--local-dir "src/prog-mode.el"))
(load-file (concat gajo--local-dir "src/move-buffer.el"))
(load-file (concat gajo--local-dir "src/kill-all-buffers.el"))

;;; .emacs ends here
