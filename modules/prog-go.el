;;; prog-go.el --- Go support via Eglot -*- lexical-binding: t; -*-

;;; Commentary:

;; Go via Eglot, using `gopls' as the LSP server.
;;
;; `https://github.com/golang/tools/blob/master/gopls/doc/emacs.md'
;;
;; Update PATH after installing the go binary, e.g.:
;;   export PATH="$PATH:/usr/local/go/bin:/home/gajo/go/bin"

;;; Code:

(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode))

(add-hook 'go-mode-hook #'eglot-ensure)

;; Format the buffer and organize imports on save. Uses
;; `gajo--eglot-format-buffer-safely' (core/core-programming.el) so a
;; flaky LSP request doesn't pop the debugger on every idle `super-save'
;; autosave; `organize-imports' gets the same treatment for the same
;; reason.
(add-hook 'before-save-hook
  (lambda ()
    (when (derived-mode-p 'go-mode)
      (gajo--eglot-format-buffer-safely)
      (when (eglot-managed-p)
        (condition-case err
            (call-interactively #'eglot-code-action-organize-imports)
          (error (message "Eglot organize-imports failed: %s" (error-message-string err))))))))

;;; prog-go.el ends here
