;;; prog-csharp.el --- C# support via Eglot -*- lexical-binding: t; -*-

;;; Commentary:

;; C# major mode plus Eglot, using `csharp-ls' as the LSP server.
;;
;; `csharp-mode' ships built into Emacs as of version 29 -- do NOT
;; `:straight t' it, that fetches a shadow copy that collides with the
;; built-in one and throws a "please delete this package" warning.
;;
;; Setup required outside Emacs:
;;
;; 1. Install the LSP server as a global dotnet tool:
;;      dotnet tool install --global csharp-ls
;; 2. Make sure it's on PATH (e.g. in .bashrc):
;;      export PATH="$PATH:/home/gajo/.dotnet/tools"

;;; Code:

(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(add-hook 'csharp-mode-hook #'eglot-ensure)

;; Format on save via the LSP server, same pattern as F#/Go. See
;; `gajo--eglot-format-buffer-safely' in core/core-programming.el for why
;; this doesn't call `eglot-format-buffer' directly.
(add-hook 'before-save-hook
  (lambda ()
    (when (derived-mode-p 'csharp-mode)
      (gajo--eglot-format-buffer-safely))))

;;; prog-csharp.el ends here
