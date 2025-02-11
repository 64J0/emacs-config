;;; prog-mode.el --- My programming setup -*- lexical-binding: t; -*-

;;; Commentary:

;; GENERAL PROGRAMMING
;;
;; Language Server Protocol Support for Emacs
;;
;; Aims to provide IDE-like experience by providing optional integration with
;; the most popular Emacs packages like company, flycheck and projectile.
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
;; - flycheck
;; - lsp-ui
;; - lsp-ivy
;; - lsp-treemacs
;; - neotree
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
;; - clojure-mode
;; - rider
;; - local erlang stuff
;; - magit

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
         (c++-mode       . lsp-deferred)
         (erlang-mode    . lsp-deferred)
         (clojure-mode   . lsp-deferred))
  :config
  ;; performance tuning
  (setq gc-cons-threshold (* 100 1024 1024)
        ;; warn when opening files bigger than 100MB
        large-file-warning-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024) ;; 1mb
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 1.0
        lsp-log-io nil ;; if set to true can cause a performance hit
        treemacs-space-between-root-nodes nil)
  ;; UI
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-modeline-diagnostics-enable t
        ;; :global/:workspace/:file
        lsp-modeline-diagnostics-scope :workspace)
  ;; for checking the compilation `lsp-treemacs-errors-list'
  (setq lsp-modeline-code-actions-mode nil)
  ;; F# ---------------------------------
  ;; `https://emacs-lsp.github.io/lsp-mode/page/lsp-fsharp/'
  (setq lsp-fsharp-enable-reference-code-lens t)
  (setq lsp-fsharp-server-install-dir "/home/gajo/.dotnet/tools/")
  (setq lsp-fsharp-use-dotnet-tool-for-fsac t)
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
  ;; There's something wrong with the ELP installation through lsp, so it was
  ;; required to install the package manually from:
  ;; `https://github.com/WhatsApp/erlang-language-platform/releases'.
  ;;
  ;; More informations regarding this tool:
  ;; `https://whatsapp.github.io/erlang-language-platform/docs/get-started/editors/emacs/'
  ;;
  ;; Other tips:
  ;;
  ;; 1. After setting this configuration, LSP and ELP were trying to add file
  ;; watchers to all my files down the Desktop/ folder. I was able to make it
  ;; work properly; ie look for the files only from my project, using the
  ;; command `lsp-workspace-remove-all-folders'. Then, after reopeing the Erlang
  ;; codebase, it asked me for the root directory, which I provided
  ;; interactively.
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("elp" "server"))
                    :major-modes '(erlang-mode)
                    :priority 0
                    :server-id 'erlang-language-platform))
  )

;; Modern on-the-fly syntax checking extension for GNU Emacs.
;;
;; Repository: `https://github.com/flycheck/flycheck'
(use-package flycheck
  :straight t
  :init (global-flycheck-mode +1))

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

;; Integration between lsp-mode and treemacs and implementation of treeview
;; controls using treemacs as a tree renderer.
;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  :straight t)

;; A Emacs tree plugin like NerdTree for Vim.
;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :straight t
  :bind
  ("C-b" . gajo--neotree-refresh-and-toggle) ;; the same from vs code
  :config
  (defun gajo--neotree-refresh-and-toggle ()
    "Automate the refresh of neotree when it's toggled."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-toggle)
      (neotree-refresh 1))))

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
;; Some useful commands and tips:
;;
;; - Search for patterns at the project: `projectile-grep'
;; - Remove files from index with `.projectile' file
;; - Search for a project with `projectile-find-file'
;; - Search for a directory `projectile-find-dir'
;; - Switch projects `projectile-switch-project'
(use-package projectile
  :straight t
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ;; Recommended keymap prefix on macOS
              ("s-p"   . projectile-command-map)
              ;; Recommended keymap prefix on Windows/Linux
              ("C-c p" . projectile-command-map))
  :config
  ;; .NET C# or F# projects
  (projectile-register-project-type 'dotnet #'projectile-dotnet-project-p
                                    :project-file '("?*.csproj" "?*.fsproj")
                                    :compile "dotnet build"
                                    :run "dotnet run"
                                    :test "dotnet test"))

;; This mode sets up hooks so that EditorConfig properties will be loaded and
;; applied to the new buffers automatically when visiting files.
;;
;; `https://github.com/editorconfig/editorconfig-emacs'
;;
(use-package editorconfig
  :straight t
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

;; Highlight uncommited changes on the left side of the window
;; area known as the "gutter"
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; A git blame plugin for emacs inspired by VS Code’s GitLens plugin and Vim
;; plugin.
;;
;; - `https://github.com/Artawower/blamer.el'
(use-package blamer
  :straight t
  :diminish blamer-mode
  :bind (("s-i" . blamer-show-commit-info))
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

;; YASnippet is a template system for Emacs. It allows you to type an
;; abbreviation and automatically expand it into function templates.
;; `https://github.com/joaotavora/yasnippet'
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
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
;;
;; Provides support for the F# language in Emacs
;; `https://github.com/fsharp/emacs-fsharp-mode'
;;
;; For LSP to work properly we need to install the fsautocomplete package as a
;; global package:
;;
;; - `dotnet tool install --global fsautocomplete'
;;
;; Then, add it to the PATH, for example (.bashrc):
;;
;; `export PATH="$PATH:/home/gajo/.dotnet/tools"'
;;
(use-package fsharp-mode
  :defer t
  :straight t
  :mode (("\\.fs$"     .  fsharp-mode)
	 ("\\.fsx$"    .  fsharp-mode)
	 ("\\.fsi$"    .  fsharp-mode)
         ("\\.fsproj$" .  xml-mode)
         ("\\.csproj$" .  xml-mode))
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

;; Clojure programming
;; `https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/'
;;
;; To work with the lsp, it is required to install the clojure-server lsp:
;;
;; - M-x lsp-install-server => clojure-lsp
;;
(use-package clojure-mode
  :straight t
  :mode ("\\.clj\\'" . clojure-mode))

;; Clojure REPL
;; `https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/'
;;
(use-package cider
  :straight t)

;; Erlang configuration
;;
;; Notice that I installed it using `https://github.com/kerl/kerl';
;;
;; $ kerl build 27.1.2
;; $ kerl install 27.1.2 /home/gajo/lib/erlang/27.1.2
;; $ . /home/gajo/lib/erlang/27.1.2/activate
;;
;; About erlang-mode:
;; `https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html'
;;
;; Another tutorial:
;; `https://alexott.net/en/writings/emacs-devenv/EmacsErlang.html'
;;
(setq load-path (cons "/home/gajo/lib/erlang/27.1.2/lib/tools-4.1/emacs" load-path))
(setq exec-path (cons "/home/gajo/lib/erlang/27.1.2/bin" exec-path))
(setq-default erlang-root-dir "/home/gajo/lib/erlang/27.1.2")
(setq-default erlang-man-root-dir "/home/gajo/lib/erlang/27.1.2/man")
(require 'erlang-start "/home/gajo/lib/erlang/27.1.2/lib/tools-4.1/emacs/erlang-start.el")
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

;; Magit
;;
;; Magit is an interface to the version control system Git, implemented as an
;; Emacs package. Magit aspires to be a complete Git porcelain.
;;
;; - `https://magit.vc/manual/magit/index.html'
;; - `https://emacsair.me/2017/09/01/magit-walk-through/'
(use-package magit
  :straight t)

;;; prog-mode.el ends here
