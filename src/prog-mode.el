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
;; Table of packages:
;;
;; - lsp-mode
;; - lsp-ui
;; - lsp-ivy
;; - helm-lsp
;; - dap-mode
;; - projectile
;; - editorconfig
;; - diff-hl
;; - yasnippet
;; - fsharp-mode
;; - python-mode
;; - json-mode
;; - dockerfile-mode
;; - terraform-mode
;; - yaml-mode
;; - markdown-mode
;; - sml-mode

;;; Code:

(require 'use-package)

;; About `lsp-deferred':
;; https://github.com/emacs-lsp/lsp-mode/discussions/3360
(use-package lsp-mode
  :defer t
  :straight t
  :hook ((lsp-mode       . lsp-headerline-breadcrumb-mode)
         (yaml-mode      . lsp-deferred)
         (fsharp-mode    . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (python-mode    . lsp-deferred)
         (sh-mode        . lsp-deferred)
         (sql-mode       . lsp-deferred)
         (c-mode         . lsp-deferred)
         (c++-mode       . lsp-deferred))
  :config
  ;; performance tuning
  (setq gc-cons-threshold 100000000
        ;; warn when opening files bigger than 100MB
        large-file-warning-threshold 100000000
        read-process-output-max (* 1024 1024) ;; 1mb
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 1.0
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
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

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

;; This package provides alternative of the build-in lsp-mode xref-appropos
;; which provides as you type completion.
;; `https://github.com/emacs-lsp/helm-lsp'
(use-package helm-lsp
  :straight t)

;; Emacs client/library for Debug Adapter Protocol is a wire protocol for
;; communication between client and Debug Server. It's similar to the LSP but
;; provides integration with debug server.
;; `https://github.com/emacs-lsp/dap-mode'
(use-package dap-mode
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
              ("s-p"   . projectile-command-map)
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

;; YASnippet is a template system for Emacs. It allows you to type an
;; abbreviation and automatically expand it into function templates.
;; `https://github.com/joaotavora/yasnippet'
(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs
        (list (concat gajo--local-dir "yasnippets")))
  (yas-global-mode 1))

;; ======================================================
;; C++ CONFIG
;; `https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/'
;; `https://github.com/atilaneves/cmake-ide'
;;
;; - LSP server: `clangd'

;; ======================================================
;; F# CONFIG
;; Got this configuration from Magueta's config
;; `https://github.com/MMagueta/MageMacs/blob/macintosh/init.el'
;;
;; Provides support for the F# language in Emacs
;; `https://github.com/fsharp/emacs-fsharp-mode'
;;
(use-package fsharp-mode
  :defer t
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
  ;; https://github.com/fsharp/emacs-fsharp-mode/tree/master#compiler-and-repl-paths
  (setq inferior-fsharp-program "dotnet fsi --readline-"))

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

;; SML Mode
(use-package sml-mode
  :straight t
  :mode ("\\.sml\\'" . sml-mode))

;;; prog-mode.el ends here
