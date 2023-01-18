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
  (setq lsp-fsharp-auto-workspace-init nil
        lsp-fsharp-enable-reference-code-lens t
        lsp-fsharp-external-autocomplete t
        lsp-fsharp-generate-binlog nil
        lsp-fsharp-interface-stub-generation t
        lsp-fsharp-keywords-autocomplete t
        lsp-fsharp-linter nil
        lsp-fsharp-record-stub-generation t
        lsp-fsharp-resolve-namespaces t
        lsp-fsharp-server-args nil)
  ;; Terraform --------------------------
  (setq lsp-terraform-ls-enable-show-reference t
        lsp-semantic-tokens-enable t
        lsp-semantic-tokens-honor-refresh-requests t
        lsp-enable-links t)
  ;; YAML -------------------------------
  (setq lsp-yaml-bracket-spacing t
        lsp-yaml-completion t
        lsp-yaml-format-enable t
        lsp-yaml-hover t
        lsp-yaml-single-quote nil
        lsp-yaml-validate t)
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

;; Integration between lsp-mode and treemacs and implementation of treeview
;; controls using treemacs as a tree renderer.
;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  :straight t
  :after lsp
  :commands lsp-treemacs-errors-list)

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

;; Is a complete text-based user interface to Git.
;; https://magit.vc/
;; (use-package magit
;;   :straight t)

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
  :defer t
  :mode ("\\Dockerfile\\'" "\\.dockerfile\\'"))

;; YAML mode to handle YAML manifests.
;; `https://www.emacswiki.org/emacs/YamlMode'
;; 
(use-package yaml-mode
  :straight t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Terraform mode to handle Terraform code.
;; `https://github.com/emacsorphanage/terraform-mode'
;;
(use-package terraform-mode
  :straight t
  :defer t
  :mode ("\\.tf\\'" . terraform-mode)
  :hook (terraform-mode-hook . terraform-format-on-save-mode)
  :config
  (setq terraform-indent-level 2))
