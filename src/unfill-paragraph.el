;;; unfill-paragraph.el --- unfill-paragraph function definition -*- lexical-binding: t; -*-

;;; Commentary:

;; https://www.emacswiki.org/emacs/UnfillParagraph
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph

;;; Code:

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph (`REGION') and makes it into a
single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;; unfill-paragraph.el ends here
