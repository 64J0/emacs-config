;;; kill-all-buffers.el --- custom kill-all-buffers function -*- lexical-binding: t; -*-

;;; Commentary:

;; Use this function to kill all the buffers open, leaving only the scratch
;; open.

;;; Code:

(defun gajo--kill-all-buffers ()
  "Kill all open buffers leaving only SCRATCH open.
Its code is inspired by the `kill-matching-buffers'."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((no-ask 1)
          (name (buffer-name buffer)))
      (when (not (string-equal name "*scratch*"))
        (funcall (if no-ask 'kill-buffer 'kill-buffer-ask) buffer)))))

;; For debugging
;;
;; (dolist (buffer (buffer-list))
;;     (let ((name (buffer-name buffer)))
;;       (when (not (not (string-equal name "*scratch*")))
;;         (message "This name is: %s" name))))

;;; kill-all-buffers.el ends here
