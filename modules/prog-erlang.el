;;; prog-erlang.el --- Erlang support via Eglot -*- lexical-binding: t; -*-

;;; Commentary:

;; Erlang via Eglot. Installed locally with `https://github.com/kerl/kerl':
;;
;;   kerl build 27.1.2
;;   kerl install 27.1.2 /home/gajo/lib/erlang/27.1.2
;;   . /home/gajo/lib/erlang/27.1.2/activate
;;
;; About erlang-mode: `https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html'
;; Tutorial: `https://alexott.net/en/writings/emacs-devenv/EmacsErlang.html'

;;; Code:

(setq load-path (cons "/home/gajo/lib/erlang/27.1.2/lib/tools-4.1/emacs" load-path))
(setq exec-path (cons "/home/gajo/lib/erlang/27.1.2/bin" exec-path))
(setq-default erlang-root-dir "/home/gajo/lib/erlang/27.1.2")
(setq-default erlang-man-root-dir "/home/gajo/lib/erlang/27.1.2/man")
(require 'erlang-start "/home/gajo/lib/erlang/27.1.2/lib/tools-4.1/emacs/erlang-start.el")
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
(add-hook 'erlang-mode-hook #'eglot-ensure)

;;; prog-erlang.el ends here
