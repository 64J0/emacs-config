;; ======================================================
;; GENERAL PROGRAMMING
;; Language Server Protocol Support for Emacs
;; Aims to provide IDE-like experience by providing optional integration
;; with the most popular Emacs packages like comapny, flycheck and
;; projectile.
;;
;; `https://github.com/emacs-lsp/lsp-mode'
;; `https://emacs-lsp.github.io/lsp-mode/'
;;
;; You need first, `lsp-mode', that is Emacs client for an LSP server. Then
;; you need to install the specific LSP server for your language. Finally,
;; call `M-x lsp' or use the corresponding major mode hook to autostart
;; the server.
;;
;; Use `M-x lsp-doctor' to validate if your `lsp-mode' is properly
;; configured.
;;
(use-package lsp-mode
  :straight t
  :hook ((lsp-mode        . lsp-headerline-breadcrumb-mode)
         (terraform-mode  . lsp-deferred) ;; sudo apt install terraform-ls
         (yaml-mode       . lsp-deferred)
         (dockerfile-mode . lsp-deferred)
         (sh-mode         . lsp-deferred))
  :config
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
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
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

;; This package contains all the higher level UI modules of lsp-mode, like
;; flycheck support and code lenses.
;; https://emacs-lsp.github.io/lsp-ui/#intro
(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
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
;; TYPESCRIPT CONFIG
;;
;; `https://github.com/emacs-typescript/typescript.el'
;; `https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/'
;;
(use-package typescript-mode
  :straight t
  :mode ("\\.ts[x]?\\'" . typescript-mode)
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

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
   :mode ("\\.fs[x]?[i]?[proj]?\\'" . fsharp-mode)
   :hook (fsharp-mode . lsp-deferred)
   :bind
   (("C-c C-,"     . 'fsharp-shift-region-left)
    ("C-c C-."     . 'fsharp-shift-region-right)
    ("C-o"         . 'fsharp-newline-and-indent)
    ("C-c C-i"     . 'run-fsharp)
    ("C-c C-a"     . 'fsharp-find-alternate-file)
    ("M-h"         . 'fsharp-mark-phrase))
   :config
   ;; (setq compile-command "dotnet watch run")
   ;; (setq inferior-fsharp-program "dotnet fsi")
   (add-hook 'fsharp-mode-hook 'highlight-indentation-mode))

;; ======================================================
;; PYTHON
;; `https://www.emacswiki.org/emacs/PythonProgrammingInEmacs'
;;
(use-package python-mode
  :straight t
  :after flycheck
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-offset 4)
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
  :config
  (setq org-babel-clojure-backend 'cider))

;; ======================================================
;; DEVSECOPS
;; Used for json files.
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
