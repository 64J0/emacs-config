;;; unfill-paragraph.el --- unfill-paragraph function definition -*- lexical-binding: t; -*-

;;; Commentary:

;; It is the opposite of fill-paragraph.
;;
;; https://www.emacswiki.org/emacs/UnfillParagraph
;; Stefan Monnier <foo at acm.org>.

;;; Code:

(defun unfill-paragraph (&optional region)
  "Opposite of fill-paragraph.  Takes a multi-line
paragraph (`REGION') and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;; unfill-paragraph.el ends here
