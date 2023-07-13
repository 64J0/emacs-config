;;; prog-mode.el --- My programming setup -*- lexical-binding: t; -*-

;;; Commentary:

;; GENERAL PROGRAMMING
;;
;; Language Server Protocol Support for Emacs
;;
;; Aims to provide IDE-like experience by providing optional integration with
;; the most popular Emacs packages like comapny, flycheck and projectile.
;;
;; + https://github.com/emacs-lsp/lsp-mode
;; + https://emacs-lsp.github.io/lsp-mode/
;;
;; You need first, `lsp-mode', that is Emacs client for an LSP server.  Then you
;; need to install the specific LSP server for your language.  Finally, call
;; `M-x lsp' or use the corresponding major mode hook to autostart the server.
;;
;; Use `M-x lsp-doctor' to validate if your `lsp-mode' is properly
;; configured.
;;

;;; Code:

(use-package lsp-mode
  :straight t
  :hook ((lsp-mode       . lsp-headerline-breadcrumb-mode)
         (yaml-mode      . lsp-deferred)
         (fsharp-mode    . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (python-mode    . lsp-deferred)
         (go-mode        . lsp-deferred)
         (sh-mode        . lsp-deferred)
         (rust-mode      . lsp-deferred))
  :config
  ;; performance tuning
  (setq gc-cons-threshold 100000000
        ;; warn when opening files bigger than 100MB
        large-file-warning-threshold 100000000
        read-process-output-max (* 1024 1024) ;; 1mb
        lsp-idle-delay 1.000
        lsp-log-io nil) ; if set to true can cause a performance hit
  ;; UI
  (setq lsp-headerline-breadcrumb-enable t)
  ;; F# ---------------------------------
  ;; `https://emacs-lsp.github.io/lsp-mode/page/lsp-fsharp/'
  (setq lsp-fsharp-enable-reference-code-lens t)
  ;; Terraform --------------------------
  ;; `https://emacs-lsp.github.io/lsp-mode/page/lsp-terraform-ls/'
  (setq lsp-disabled-clients '(tfls)
        lsp-terraform-ls-enable-show-reference t
        lsp-semantic-tokens-enable t
        lsp-semantic-tokens-honor-refresh-requests t
        lsp-enable-links t)
  ;; YAML -------------------------------
  ;; `https://emacs-lsp.github.io/lsp-mode/page/lsp-yaml/'
  ;; Only default values
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  :custom
  ;; RUST -------------------------------
  ;; `https://robert.kra.hn/posts/rust-emacs-setup/'
  (lsp-eldoc-render-all nil)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil))

;; This package contains all the higher level UI modules of lsp-mode, like
;; flycheck support and code lenses.
;; https://emacs-lsp.github.io/lsp-ui/#intro
(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-diagnostic-max-lines 7))

;; Search for some string pattern in the project.
(use-package lsp-ivy
  :straight t)

;; Projectile is a project interaction library for Emacs. Its goal is to provide
;; a nice set of features operating on a project level without introducing
;; external dependencies (when feasible).
;;
;; `https://github.com/bbatsov/projectile'
;; `https://docs.projectile.mx/projectile/installation.html'
;;
(use-package projectile
  :straight t
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config (projectile-mode +1))

;; This mode sets up hooks so that EditorConfig properties will be loaded and
;; applied to the new buffers automatically when visiting files.
;;
;; `https://github.com/editorconfig/editorconfig-emacs'
;;
(use-package editorconfig
  :straight t
  :config (editorconfig-mode 1))

;; Highlight uncommited changes on the left side of the window
;; area known as the "gutter"
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; ======================================================
;; F# CONFIG
;; Got this configuration from Magueta's config
;; `https://github.com/MMagueta/MageMacs/blob/macintosh/init.el'
;;
;; Provides support for the F# language in Emacs
;; `https://github.com/fsharp/emacs-fsharp-mode'
;;
(use-package fsharp-mode
   :straight t
   :mode (("\\.fs$"     .  fsharp-mode)
	  ("\\.fsx$"    .  fsharp-mode)
	  ("\\.fsi$"    .  fsharp-mode)
          ("\\.fsproj$" .  xml-mode))
   :hook (fsharp-mode . lsp-deferred)
   :bind
   (("C-c C-,"     . 'fsharp-shift-region-left)
    ("C-c C-."     . 'fsharp-shift-region-right)
    ("C-o"         . 'fsharp-newline-and-indent)
    ("C-c C-i"     . 'run-fsharp)
    ("C-c C-a"     . 'fsharp-find-alternate-file)
    ("M-h"         . 'fsharp-mark-phrase))
   :config
   (setq compile-command "dotnet watch run")
   (setq inferior-fsharp-program "dotnet fsi"))

;; ======================================================
;; PYTHON
;; `https://www.emacswiki.org/emacs/PythonProgrammingInEmacs'
;;
(use-package python-mode
  :straight t
  :after flycheck
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-offset 2)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))

;; Python code
;; to fix problems: `https://www.higithub.com/jorgenschaefer/issue/elpy/1936'
;; M-x elpy-rpc-reinstall-virtualenv
(use-package elpy
  :straight t
  :init
  (elpy-enable))

;; ======================================================
;; CLOJURE
;; `https://clojure.org/guides/editors'
;; `https://www.braveclojure.com/basic-emacs/'
;;
(use-package clojure-mode
  :straight t
  :mode ("\\.clj\\'" . clojure-mode)
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :straight t
  :after clojure-mode
  :config
  (setq org-babel-clojure-backend 'cider))

;; ======================================================
;; RUST LANG
;; `https://robert.kra.hn/posts/rust-emacs-setup/'
(use-package rustic
  :straight t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; ======================================================
;; GO LANG
;; `https://github.com/dominikh/go-mode.el'
;; `https://emacs-lsp.github.io/lsp-mode/manual-language-docs/lsp-gopls/'
;;
(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode))

;; ======================================================
;; DEVSECOPS
;; Used for json files.
;; `https://github.com/joshwnj/json-mode'
;;
(use-package json-mode
  :straight t
  :mode ("\\.json\\'" . json-mode))

;; Pretty syntax highlight for editing Dockerfiles.
;; `https://github.com/spotify/dockerfile-mode'
;;
(use-package dockerfile-mode
  :straight t
  :mode ("\\Dockerfile\\'" "\\.dockerfile\\'"))

;; Terraform mode to handle Terraform code.
;; `https://github.com/emacsorphanage/terraform-mode'
;;
(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\'" . terraform-mode)
  :hook (terraform-mode-hook . terraform-format-on-save-mode)
  :config
  (setq terraform-indent-level 2))

;; YAML mode to handle YAML manifests.
;; `https://www.emacswiki.org/emacs/YamlMode'
;; 
(use-package yaml-mode
  :straight t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Markdown mode with GitHub flavor.
;; `https://jblevins.org/projects/markdown-mode/'
;;
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; ===========================================================
;; Nix

;; Nix Language server, an incremental analysis assistent for writing in Nix.
;; `https://github.com/oxalica/nil'
;;
;; `https://github.com/oxalica/nil/issues/95'
;; (use-package lsp-nix
;;   :ensure lsp-mode
;;   :after (lsp-mode)
;;   :demand t
;;   :custom
;;   (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :straight t
  :mode ("\\.nix\\'" . nix-mode)
  :hook (nix-mode . lsp-deferred))

;;; prog-mode.el ends here
