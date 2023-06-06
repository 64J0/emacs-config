;;; move-buffer.el --- custom move-buffer function -*- lexical-binding: t; -*-

;;; Commentary:

;; I use this function to move a buffer to another directory while keeping the
;; same name.

;;; Code:

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

;;; move-buffer.el ends here

