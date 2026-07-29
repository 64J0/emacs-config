;;; prog-fsharp.el --- F# support via Eglot -*- lexical-binding: t; -*-

;;; Commentary:

;; F# major mode plus Eglot, using FsAutoComplete as the LSP server.
;;
;; Setup required outside Emacs:
;;
;; 1. Install the LSP server as a global dotnet tool:
;;      dotnet tool install --global fsautocomplete
;; 2. Make sure it's on PATH (e.g. in .bashrc):
;;      export PATH="$PATH:/home/gajo/.dotnet/tools"
;;
;; Repository: `https://github.com/fsharp/emacs-fsharp-mode'

;;; Code:

(use-package fsharp-mode
  :defer t
  :straight t
  :mode (("\\.fs$"     . fsharp-mode)
         ("\\.fsx$"    . fsharp-mode)
         ("\\.fsi$"    . fsharp-mode)
         ("\\.fsproj$" . xml-mode)
         ("\\.csproj$" . xml-mode))
  :bind
  (("C-c C-," . fsharp-shift-region-left)
   ("C-c C-." . fsharp-shift-region-right)
   ("C-o"     . fsharp-newline-and-indent)
   ("C-c C-i" . run-fsharp)
   ("C-c C-a" . fsharp-find-alternate-file)
   ("M-h"     . fsharp-mark-phrase))
  :config
  (setq compile-command "dotnet watch run")
  ;; https://github.com/fsharp/emacs-fsharp-mode/tree/master#compiler-and-repl-paths
  (setq inferior-fsharp-program "dotnet fsi --readline-")
  (setq fsharp-indent-offset 2)
  ;; Let each F# project resolve its own Emacs project root instead of a
  ;; single global one.
  (remove-hook 'project-find-functions #'fsharp-mode-project-root))

(use-package eglot-fsharp
  :straight t
  :after fsharp-mode
  :config
  ;; Pinned manually: `eglot-fsharp' otherwise calls
  ;; `eglot-fsharp--latest-version', which fails locally because
  ;; `json-parse-buffer' isn't available.
  ;; https://github.com/fsharp/emacs-fsharp-mode/issues/353
  (setq eglot-fsharp-server-install-dir nil)
  (setq eglot-fsharp-server-version "0.79.2"))

;; Actually attach Eglot to F# buffers -- same pattern as every other
;; language module in this config.
(add-hook 'fsharp-mode-hook #'eglot-ensure)

;; Format on save via the LSP server (FsAutoComplete formats through
;; Fantomas), mirroring the Go module's format-on-save hook. See
;; `gajo--eglot-format-buffer-safely' in core/core-programming.el for why
;; this doesn't call `eglot-format-buffer' directly.
(add-hook 'before-save-hook
  (lambda ()
    (when (derived-mode-p 'fsharp-mode)
      (gajo--eglot-format-buffer-safely))))

;;; prog-fsharp.el ends here
