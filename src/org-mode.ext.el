;;; org-mode.ext.el --- Custom org-mode extensions -*- lexical-binding: t; -*-
;;; Commentary:

;; ORG MODE EXTENSIONS
;;
;; This package presents custom extensions to improve my org-mode experience.
;;
;; =====================================================
;; ## "disable" an org-drill card interactively during an active session.
;;
;; Explanation:
;;
;; Since `org-drill' uses `read-char-choice' to capture your keystrokes while
;; reviewing a card, you can use an `:around' advice to inject a custom
;; `[d]isable' choice.
;;
;; When you press `d', the code will quietly strip the `:drill:' tag from the
;; current card and return a fake `s' (skip) character to `org-drill', forcing
;; it to seamlessly move to the next item.

;;; Code:

(require 'org)

(defvar gajo--org-drill-session-active nil
  "Dynamic flag to track if an interactive org-drill entry is currently prompting.")

;; 1. Flag when we are inside an active card session
(defun gajo--org-drill-entry-ambient-advice (orig-fun &rest args)
  "Around-advice to flag when we are actively inside an org-drill card presentation."
  (let ((gajo--org-drill-session-active t))
    (apply orig-fun args)))

(advice-add 'org-drill-entry :around #'gajo--org-drill-entry-ambient-advice)


;; 2. Intercept and rewrite the prompt text in the echo area
(defun gajo--org-drill-message-advice (orig-fun format-string &rest args)
  "Intercept org-drill's printed prompts to display the d=disable option."
  (if (and gajo--org-drill-session-active
           (stringp format-string)
           (string-match-p "s=skip" format-string))
      (let ((new-format (replace-regexp-in-string "s=skip" "s=skip, d=disable" format-string)))
        (apply orig-fun new-format args))
    (apply orig-fun format-string args)))

(advice-add 'message :around #'gajo--org-drill-message-advice)


;; 3. Catch the 'd' keypress, strip the tags, and trigger a skip
(defun gajo--org-drill-input-interceptor (orig-fun &rest args)
  "Catch the 'd' key during org-drill sessions to disable the current card."
  (let ((char (apply orig-fun args)))
    (if (and gajo--org-drill-session-active
             (or (eq char ?d) (eq char ?D)))
        (progn
          (save-excursion
            (org-back-to-heading t)
            (org-toggle-tag "drill" 'off)
            (org-toggle-tag "drill_disabled" 'on)
            (org-entry-put (point) "DRILL_DISABLED" "t"))
          (message "Card disabled dynamically!")
          (sit-for 0.6) ;; Brief pause so you can see the confirmation message
          ?s) ;; Feed '?s' back to org-drill to seamlessly skip to the next card
      char)))

;; Cover all bases for low-level input readers
(advice-add 'read-char :around #'gajo--org-drill-input-interceptor)
(advice-add 'read-char-exclusive :around #'gajo--org-drill-input-interceptor)
(advice-add 'read-key :around #'gajo--org-drill-input-interceptor)

;;; org-mode.ext.el ends here
