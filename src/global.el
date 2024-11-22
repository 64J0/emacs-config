;;; global.el --- My global configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Just used to set some default values to make Emacs look and behave the way I
;; want.

;;; Code:

;; https://www.emacswiki.org/emacs/LineNumbers
(global-display-line-numbers-mode 1) ;; show the line number
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
(delete-selection-mode t)

;; https://posts.tonyaldon.com/2022-03-05-i-bet-you-use-hl-line-mode/
(global-hl-line-mode t)

;; Use the Auto Revert mode to keep a buffer sync with respect to its visited
;; file on disk, which is useful when the file is changed by another program
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
(global-auto-revert-mode t)

(defvar emacs-user (getenv "USER") "Computer user from env.")
(message "[+] Hello %s. Starting Emacs version %s" emacs-user emacs-version)

;; =======================================================================
;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key [f5] 'find-alternate-file) ;; reload a file

;;; global.el ends here
