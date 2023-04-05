;;; global.el --- My global configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Just used to set some default values to make Emacs look and behave the way I
;; want.

;;; Code:

(global-linum-mode) ;; show the line number
(tool-bar-mode -1) ;; remove tool bar
(menu-bar-mode -1) ;; remove menu bar
(set-face-attribute 'default nil
		    :height 140
		    :family "DejaVu Sans Mono") ;; font size and family
(setq user-full-name "Vinícius Gajo"
      inhibit-startup-message t
      standard-indent 4
      auto-save-no-message t
      column-number-mode t ;; show coordinates (y, x)
      delete-selection-mode t ;; delete text when selected and start typing
      system-time-locale "pt_BR.UTF-8" ;; set encode
      make-backup-files nil ;; avoid "~" files
      initial-buffer-choice "~/org/activities.org")
(setq-default indent-tabs-mode nil
              fill-column 80)

;; Delete highlighted text
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Replacing-highlighted-text.html
(delete-selection-mode 1)

(defvar emacs-user (getenv "USER") "Computer user from env.")
(message "[+] Hello %s. Starting Emacs version %s" emacs-user emacs-version)

;; =======================================================================
;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key [f5] 'find-alternate-file) ;; reload a file

;;; global.el ends here
