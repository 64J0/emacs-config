;;; init.el --- My custom Emacs setup -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my personal Emacs configuration file.
;;
;; Structure is Prelude-inspired (`https://github.com/bbatsov/prelude'):
;;
;; - `core/'    always-loaded config: completion, UI, editor, general
;;   programming tooling.  Not optional.
;; - `modules/' one file per language/topic.  Toggle a language on/off by
;;   (un)commenting its entry in `modules/modules-list.el' -- no need to
;;   delete the file itself.
;;
;; Each file documents its own packages in its `;;; Commentary:' header.

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

;; Pull in the newer `org' from straight before anything else has a chance
;; to trigger loading Emacs's built-in Org first -- otherwise `use-package
;; org' in modules/lang-org.el loads too late and every Org buffer starts
;; with an "Org version mismatch" warning.
;; https://orgmode.org/worg/org-faq.html#mixed-installation
(straight-use-package 'org)

;; https://github.com/radian-software/straight.el#how-do-i-update-melpa-et-al
(defalias 'straight-update-all-packages 'straight-pull-all)
;; https://github.com/radian-software/straight.el#how-do-i-uninstall-a-package
(defalias 'straight-clean-packages 'straight-remove-unused-repos)
(setq straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake))

;; ============================================
;; Load external configuration
(message "Loading external configuration")

(setq debug-on-error t)

(defconst gajo--local-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Base path for this config, computed from this file's own location.")

(defconst gajo--core-files
  '("core/core-completion.el"
    "core/core-ui.el"
    "core/core-editor.el"
    "core/core-programming.el")
  "Always-loaded config files, in load order.")

(defun gajo--load-files (file-paths)
  "Load each of FILE-PATHS, relative to `gajo--local-dir', in order."
  (dolist (gajo--file-path file-paths)
    (message "Loading file at: %s" gajo--file-path)
    (load-file (concat gajo--local-dir gajo--file-path))))

(gajo--load-files gajo--core-files)

;; `modules/modules-list.el' defines `gajo--module-files', the toggleable
;; per-language module list -- comment out an entry there to disable it.
(load-file (concat gajo--local-dir "modules/modules-list.el"))
(gajo--load-files gajo--module-files)

;;; init.el ends here
