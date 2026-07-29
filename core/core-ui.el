;;; core-ui.el --- Theme and visual chrome -*- lexical-binding: t; -*-

;;; Commentary:

;; Theme, modeline, and breadcrumbs -- the visual parts of the "make Emacs
;; feel like an IDE" goal. No icon fonts: text-only, so nothing here depends
;; on a Nerd Font being installed on the system.
;;
;; Table of packages:
;;
;; - modus-themes
;; - doom-modeline
;; - breadcrumb

;;; Code:

(require 'use-package)

;; `https://www.reddit.com/r/emacs/comments/j7eruf/favorite_light_themes/'
;;
;; (use-package doom-themes
;;   :straight t
;;   :init
;;   (load-theme 'doom-opera-light t))

;; `https://github.com/protesilaos/modus-themes'
;;
(use-package modus-themes
  :straight t
  :init
  (load-theme 'modus-operandi t))

;; A fancy, VS Code-like status line: git branch, diagnostics count,
;; project name, etc. Icons off, text-only, so no font dependency.
;;
;; Repository: `https://github.com/seagle0128/doom-modeline'
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon nil))

;; VS Code-style breadcrumb: shows the file path plus the enclosing
;; function/class/symbol at point, powered by Eglot/Imenu.
;;
;; Repository: `https://github.com/joaotavora/breadcrumb'
(use-package breadcrumb
  :straight t
  :init (breadcrumb-mode 1))

;;; core-ui.el ends here
