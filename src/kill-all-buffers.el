;;; kill-all-buffers.el --- custom kill-all-buffers function -*- lexical-binding: t; -*-

;;; Commentary:

;; Use this function to kill all the buffers open, leaving only the scratch
;; open.

;;; Code:

(require 'rx)

(defun gajo--kill-all-buffers ()
  "Kill all open buffers leaving only SCRATCH open."
  (interactive)
  (let ((internal-too nil)
        (no-ask t))
    (kill-matching-buffers
     (rx (zero-or-more (not (any ""))))
     internal-too
     no-ask)))

;;; kill-all-buffers.el ends here
