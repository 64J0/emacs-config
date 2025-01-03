;;; theme.el --- Emacs theme configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file holds my configuration for setting the Emacs theme.

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

;;; theme.el ends here
