;;; core-programming.el --- General programming tooling -*- lexical-binding: t; -*-

;;; Commentary:

;; Tooling shared by every programming language: LSP client, diagnostics,
;; project navigation, git integration, snippets, and a few VS Code-style
;; conveniences. Per-language setup (major modes, LSP servers) lives in
;; `modules/'.
;;
;; Table of packages:
;;
;; - eglot (built into Emacs)
;; - exec-path-from-shell
;; - flycheck
;; - editorconfig
;; - treemacs (+ treemacs-projectile, treemacs-magit)
;; - projectile
;; - diff-hl
;; - blamer
;; - yasnippet
;; - magit
;; - hl-todo
;; - avy
;; - expand-region
;; - eldoc-box

;;; Code:

(require 'use-package)

;; We don't need to install Eglot given that it's distributed within Emacs.
;; It's the LSP client used by every `modules/prog-*.el' file.
(require 'eglot)

;; Format-on-save wrapper shared by the `prog-*.el' modules that use it
;; (F#, C#, Go). LSP formatting can fail for reasons outside the buffer's
;; control (a project's tool manifest not restored, a dropped LSP
;; connection, etc); left unguarded, that error propagates through
;; `before-save-hook' and (with `debug-on-error' on) pops the debugger --
;; including on every idle `super-save' autosave, which makes a single
;; misconfigured project unusable to type in until fixed. Report and move
;; on instead.
(defun gajo--eglot-format-buffer-safely ()
  "Format the current buffer via Eglot without entering the debugger on failure."
  (when (eglot-managed-p)
    (condition-case err
        (eglot-format-buffer)
      (error (message "Eglot format-on-save failed: %s" (error-message-string err))))))

;; Imports PATH (and other env vars) from the user's login shell. A
;; GUI-launched Emacs frame frequently doesn't see PATH entries added in
;; .bashrc/.zshrc (e.g. `~/.dotnet/tools', `~/go/bin'), which silently
;; breaks Eglot's ability to find LSP servers like `fsautocomplete',
;; `csharp-ls', or `gopls' even though they work fine from a terminal.
;;
;; Repository: `https://github.com/purcell/exec-path-from-shell'
(use-package exec-path-from-shell
  :straight t
  :config
  (unless noninteractive
    (exec-path-from-shell-initialize)))

(require 'editorconfig)
(editorconfig-mode 1)

;; Modern on-the-fly syntax checking extension for GNU Emacs.
;;
;; Repository: `https://github.com/flycheck/flycheck'
(use-package flycheck
  :straight t
  :init (global-flycheck-mode +1))

;; Eglot reports diagnostics through Flymake, so once a buffer is
;; Eglot-managed, Flycheck is redundant -- turn it off there.
(add-hook 'eglot-managed-mode-hook
  (lambda () (flycheck-mode -1)))

;; A tree-style sidebar for browsing and acting on the project, like VS
;; Code's Explorer panel: file operations, git status markers, project
;; awareness.
;;
;; Repository: `https://github.com/Alexander-Miller/treemacs'
(use-package treemacs
  :straight t
  :bind
  ("C-b" . treemacs)) ;; the same as VS Code's Explorer toggle

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

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
                    :background unspecified
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

;; Magit
;;
;; Magit is an interface to the version control system Git, implemented as an
;; Emacs package. Magit aspires to be a complete Git porcelain.
;;
;; - `https://magit.vc/manual/magit/index.html'
;; - `https://emacsair.me/2017/09/01/magit-walk-through/'
(use-package magit
  :straight t)

;; Highlights TODO/FIXME/HACK/NOTE comments in eye-catching colors, like the
;; "TODO Highlight" VS Code extension.
;;
;; Repository: `https://github.com/tarsius/hl-todo'
(use-package hl-todo
  :straight t
  :init (global-hl-todo-mode))

;; Jump to any visible line/word/char in a few keystrokes.
;;
;; Repository: `https://github.com/abo-abo/avy'
(use-package avy
  :straight t
  :bind (("C-;" . avy-goto-char-timer)))

;; Grow the selected region outward by semantic units (word -> sexp ->
;; statement -> ...) with repeated presses.
;;
;; Repository: `https://github.com/magnars/expand-region.el'
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)))

;; Shows Eglot's `eldoc' hover documentation in a popup box near point
;; instead of the echo area -- closer to VS Code's hover tooltip.
;;
;; Repository: `https://github.com/casouri/eldoc-box'
(use-package eldoc-box
  :straight t
  :diminish eldoc-box-hover-mode
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

;;; core-programming.el ends here
