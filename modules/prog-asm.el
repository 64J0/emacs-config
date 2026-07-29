;;; prog-asm.el --- Assembly (NASM) support -*- lexical-binding: t; -*-

;;; Commentary:

;; NASM syntax highlighting.
;;
;; Repository: `https://github.com/skeeto/nasm-mode'

;;; Code:

(use-package nasm-mode
  :straight t)
(add-to-list 'auto-mode-alist '("\\.asm$" . nasm-mode))

;;; prog-asm.el ends here
