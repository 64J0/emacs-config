;;; prog-python.el --- Python support via Eglot -*- lexical-binding: t; -*-

;;; Commentary:

;; Python via Eglot, using `pylsp' (`python-lsp-server') as the LSP server.
;;
;; `https://www.emacswiki.org/emacs/PythonProgrammingInEmacs'
;;
;; Install the server inside a virtual environment:
;;   pip install 'python-lsp-server[all]'

;;; Code:

(use-package python-mode
  :straight t
  :after flycheck
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))

(add-hook 'python-mode-hook #'eglot-ensure)

;;; prog-python.el ends here
