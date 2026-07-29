;;; core-editor.el --- Global editor behavior -*- lexical-binding: t; -*-

;;; Commentary:

;; Default values and small interactive commands that make Emacs look and
;; behave the way I want, independent of any particular language.

;;; Code:

;; =======================================================================
;; GLOBAL SETTINGS

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
;; CUSTOM COMMANDS

;; Opposite of `fill-paragraph'.
;; https://www.emacswiki.org/emacs/UnfillParagraph
;; Stefan Monnier <foo at acm.org>.
(defun unfill-paragraph (&optional region)
  "Opposite of fill-paragraph.  Takes a multi-line
paragraph (`REGION') and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Move the current buffer's file to another directory, keeping its name.
(defun gajo--move-buffer ()
  "Move the CURRENT buffer to a DESTINATION folder."
  (interactive)
  (let* ((filename (buffer-name))
         (source-path (buffer-file-name))
         (dest-dir (read-directory-name "The DESTINATION directory: "))
         (dest-path (concat dest-dir filename)))
    (progn
      (write-file dest-path)
      (delete-file source-path)
      (message "%s moved to directory %s" filename dest-dir))))

;; Kill every buffer except `*scratch*'.
(defun gajo--kill-all-buffers ()
  "Kill all open buffers leaving only SCRATCH open.
Its code is inspired by the `kill-matching-buffers'."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((no-ask 1)
          (name (buffer-name buffer)))
      (when (not (string-equal name "*scratch*"))
        (funcall (if no-ask 'kill-buffer 'kill-buffer-ask) buffer)))))

;; =======================================================================
;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key [f5] 'find-alternate-file) ;; reload a file

;;; core-editor.el ends here
