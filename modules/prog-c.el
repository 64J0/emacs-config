;;; prog-c.el --- C/C++ support via Eglot -*- lexical-binding: t; -*-

;;; Commentary:

;; C/C++ via Eglot, using `clangd' as the LSP server.
;;
;; `https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/' (clangd setup
;; notes still apply even though this config uses Eglot, not lsp-mode)
;; `https://github.com/atilaneves/cmake-ide'

;;; Code:

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;;; prog-c.el ends here
