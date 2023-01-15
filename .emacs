;; -*- lexical-binding: t; -*-

;; Based on: https://github.com/bbatsov/prelude

;; Until Emacs 24.1 (June 2012), Elisp only had dynamically scoped variables, a
;; feature, mostly by accident, common to old lisp dialects. While dynamic scope
;; has some selective uses, it’s widely regarded as a mistake for local
;; variables, and virtually no other languages have adopted it.

(require 'cl-lib) ;; cl -> common lisp

;; =======================================================================
;; GLOBAL CONFIGURATION
(setq gajo-dir "~/Desktop/codes/emacs-config/")
(load-file (concat gajo-dir "src/global.el"))

;; =======================================================================
;; CUSTOM FUNCTIONS
;; Elisp functions I use to configure my Emacs.
(load-file (concat gajo-dir "src/unfill-paragraph.el"))

;; =======================================================================
;; PACKAGE MANAGEMENT
;; when error: M-x package-refresh-contents
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

;; =======================================================================
;; EMACS THEME
(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

;; =======================================================================
;; GENERAL USAGE
;; Displays the key bindings following your currently entered incomplete
;; command (a prefix) in a popup.
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  :init
  (setq super-save-auto-save-when-idle t))

;; Framework for incremental completions and narrowing selections.
;; https://emacs-helm.github.io/helm/
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list)))

;; Highlight delimiters such as parentheses, brackets or braces
;; according to their depth
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Show directory tree on the lateral
;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :ensure t
  :bind (("C-b" . 'neotree-toggle)))

;; Keybindings to comment line region and single line
(use-package undo-tree
  :ensure t
  :init
  (undo-tree-mode))

;; https://oremacs.com/swiper/#copying
;; https://github.com/abo-abo/swiper
;; Ivy: generic completion mechanism for Emacs
;; Counsel: collection of Ivy-enhanced versions of common Emacs commands
;; Swiper: Ivy-enhanced alternative to Isearch
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; ======================================================
;; GENERAL PROGRAMMING
;; Language Server Protocol Support for Emacs
;; Aims to provide IDE-like experience by providing optional integration
;; with the most popular Emacs packages like comapny, flycheck and
;; projectile.
;; https://github.com/emacs-lsp/lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/
;;
;; You need first, `lsp-mode', that is Emacs client for an LSP server. Then
;; you need to install the specific LSP server for your language. Finally,
;; call `M-x lsp' or use the corresponding major mode hook to autostart
;; the server.
;;
;; Use `M-x lsp-doctor' to validate if your `lsp-mode' is properly
;; configured.
(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode        . lsp-headerline-breadcrumb-mode)
         (fsharp-mode     . lsp-deferred)
         (terraform-mode  . lsp-deferred) ;; sudo apt install terraform-ls
         (yaml-mode       . lsp-deferred)
         (dockerfile-mode . lsp-deferred)
         (sh-mode         . lsp-deferred))
  :init
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
  :config
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

;; This package contains all the higher level UI modules of lsp-mode, like
;; flycheck support and code lenses.
;; https://emacs-lsp.github.io/lsp-ui/#intro
(use-package lsp-ui
  :ensure t
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
  :ensure t
  :after lsp
  :commands lsp-treemacs-errors-list)

;; Search for some string pattern in the project.
(use-package lsp-ivy
  :ensure t)

;; Puts angry red squiggles on the screen when I do something stupid.
;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; COMplete ANYthing
;; Could give wrong completions (orgmode)
;; http://company-mode.github.io/
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :commands
  (company-mode company-indent-or-complete-common)
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-selection-wrap-around t))

;; Make company box look better
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; lsp -> language server protocol
;; https://github.com/emacs-lsp/helm-lsp
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

;; https://github.com/company-mode/company-quickhelp
(use-package company-quickhelp
   :ensure t
   :init
   (setq company-tooltip-limit 10 ; bigger popup window
	 company-tooltip-minimum-width 15
	 company-tooltip-align-annotations t ; align annotations to the right tooltip border
	 company-quickhelp-delay '1.0)
   :config
   (company-quickhelp-mode nil)
   :hook
   ((emacs-lisp-mode . (lambda () (company-mode)))))

;; Is a complete text-based user interface to Git.
;; https://magit.vc/
;; (use-package magit
;;   :ensure t)

;; Highlight uncommited changes on the left side of the window
;; area known as the "gutter"
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Projectile is a project interaction library for Emacs. Its goal is to provide
;; a nice set of features operating on a project level without introducing
;; external dependencies (when feasible).
;; https://github.com/bbatsov/projectile
;; https://docs.projectile.mx/projectile/installation.html
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; This mode sets up hooks so that EditorConfig properties will be loaded and
;; applied to the new buffers automatically when visiting files.
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Minor mode for Emacs that deals with parens pairs and tries to be smart about
;; it.
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t) ;; These options can be t or nil.
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t)
  :custom-face
  (sp-show-pair-match-face ((t (:foreground "White")))) ;; Could also have :background "Grey" for example.
  )

;; ======================================================
;; TYPESCRIPT CONFIG
;; https://github.com/emacs-typescript/typescript.el
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts[x]?\\'")
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; ======================================================
;; F# CONFIG
;; Got this configuration from Magueta's config
;; https://github.com/MMagueta/MageMacs/blob/macintosh/init.el

;; Provides support for the F# language in Emacs
;; https://github.com/fsharp/emacs-fsharp-mode
(use-package fsharp-mode
   :ensure t
   :mode ("\\.fs[x]?[i]?[proj]?\\'")
   :hook ((fsharp-mode      . (lambda () (lsp))))
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
;; DEVSECOPS
;; Used for json files.
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; Pretty syntax highlight for editing Dockerfiles.
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode ("\\Dockerfile\\'" "\\.dockerfile\\'"))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'"))

;; https://github.com/emacsorphanage/terraform-mode
(use-package terraform-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  (setq terraform-indent-level 2))

;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
(use-package python-mode
  :ensure t
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))

;; Python code
;; to fix problems: https://www.higithub.com/jorgenschaefer/issue/elpy/1936
;; M-x elpy-rpc-reinstall-virtualenv
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; ORG MODE
;;
;; A GNU Emacs major mode for keeping notes, authoring documents, computational
;; notebooks, literate programming, maintaining to-do lists, planning projects,
;; and more.
;;
;; https://orgmode.org/
;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
;; https://github.com/ebellani/Emacs.d/blob/master/init.el

(defun concat-deps-path (filename)
  "This function helps to avoid repeating the full path for the
   `deps' folder (dependencies)."
  (if (string-empty-p filename)
      (error "[-] File name is empty!")
    (concat "~/org/deps/" filename)))

;; FlySpell for spell checking
;; https://www.emacswiki.org/emacs/FlySpell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(setq flyspell-issue-message-flag nil) ;; performance improvement

(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :preface
  (setq org-export-backends '(moderncv md beamer ascii html latex odt org))
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (setq gajo-org-srs-path "~/org/srs/deck-refile.org"
        gajo-org-notes-path "~/org/notes.org"
        gajo-org-refile-path "~/org/refile.org"
        gajo-org-agenda-path "~/org/agenda.org"
        gajo-org-todo-path "~/org/todo.org"
        gajo-org-work-path "~/org/work.org"
        gajo-org-meetings-path "~/org/meetings.org")
  ;; Required for PlantUML diagrams
  ;; From: https://plantuml.com/download
  (setq org-plantuml-jar-path
        (expand-file-name (concat-deps-path "plantuml-1.2021.16.jar")))
  (setq org-ditaa-jar-path
        (expand-file-name (concat-deps-path "ditaa0_9.jar")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql      . t)
     (latex    . t)
     (shell    . t)
     (python   . t)
     (js       . t)
     (plantuml . t)
     (ditaa    . t)
     (C        . t)
     (org      . t)
     (fsharp   . t)))
  (setq org-src-fontify-natively t)
  (setq org-directory "~/org")
  (setq org-agenda-files (list
                          gajo-org-agenda-path
                          gajo-org-work-path
                          gajo-org-todo-path))
  (setq org-log-done t)
  (setq org-export-backends
	'(md gfm beamer ascii taskjuggler html latex odt org))
  (setq org-support-shift-select 'always)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-babel-inline-result-wrap "=%s=")
  (setq org-use-sub-superscripts '{})
  (setq org-export-with-sub-superscripts '{})
  (setq org-duration-format '((special . h:mm)))
  (setq org-goto-interface 'outline-path-completion)
  ;; AGENDA
  (setq org-agenda-include-diary nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-sticky nil)
  (setq org-agenda-span 21)
  ;; LaTeX
  (setq org-latex-pdf-process (list "latexmk -silent -f -pdf %f"))
  (setq org-latex-create-formula-image-program 'dvipng) ; apt install dvipng
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-highlight-latex-and-related '(native))
  (setq org-cite-export-processors '((latex biblatex)
                                     (moderncv basic)
                                     (t basic)))
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)"))))
  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "orange" :weight bold)
		("NEXT" :foreground "blue" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
		("CANCELLED" :foreground "red" :weight bold))))
  (setq org-refile-targets
        `((nil                     :maxlevel . 9)
          (org-agenda-files        :maxlevel . 2)
          (,gajo-org-srs-path      :maxlevel . 2)
          (,gajo-org-agenda-path   :maxlevel . 2)))
  (setq org-capture-templates
        `(("t" "To-Do" entry (file gajo-org-todo-path)
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %t\n"
                    ":PROPERTIES:\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    " - State \"TODO\"       from \"\"  %U  \\\\\n"
                    "  %^{Initial log} %?\n"
                    ":END:")
           :jump-to-captured t)
          ("w" "Work reminder" entry (file+headline gajo-org-work-path "DevSecOps")
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %t\n"
                    ":PROPERTIES:\n"
                    ":work_reminder: t\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    "- Initial note taken on %U \\\n"
                    "%^{Initial note}\n"
                    ":END:\n"))
          ("m" "Meeting log" entry (file gajo-org-meetings-path)
           ,(concat "* %^{Title}\n"
                    "** Context\n"
                    "%^{Context}\n"
                    "** Goal\n"
                    "%^{Goal}\n"
                    "** Agenda\n"
                    "%^{Agenda}\n"
                    "** Ata\n"
                    "%^{Minutes})\n"))
          ("d" "Drill card with answer" entry (file ,gajo-org-srs-path)
           ,(concat "* Item           :drill:\n"
                    "%^{Question}\n"
                    "** Answer\n"
                    "%^{Answer}\n"))
          ("z" "Drill" entry (file ,gajo-org-srs-path)
           ,(concat "* Item           :drill:\n"
                    "%?\n"))
          ("x" "Drill cloze 2" entry (file ,gajo-org-srs-path)
           ,(concat "* Item           :drill:\n"
                    ":PROPERTIES:\n"
                    ":drill_card_type: hide2cloze\n"
                    ":END:\n"
                    "%?\n")))))

;; Second brain
;; https://www.youtube.com/watch?v=AyhPmypHDEw
;; templates from:
;; https://systemcrafters.net/build-a-second-brain-in-emacs/capturing-notes-efficiently/
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/RoamNotes")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"   . completion-at-point))
  :config
  (org-roam-db-autosync-mode))

;; Spaced repetition algorithm to conduct interactive "drill sessions",
;; using org files as sources of facts to be memorised.
;; https://orgmode.org/worg/org-contrib/org-drill.html
(use-package org-drill
  :ensure t
  :commands (org-drill)
  :init
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-scope 'directory) ;; file
  (setq org-drill-maximum-items-per-session nil)
  (setq org-drill-maximum-duration 30))

;; Prettify headings and plain lists in Org mode.
;; https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :ensure t
  :init
  (add-hook
   'org-mode-hook (lambda () (org-superstar-mode 1))))

;; This repository contains add-ons to Org.
;; https://git.sr.ht/~bzg/org-contrib
(use-package org-contrib
  :ensure t)

;; Supercharge your org daily/weekly/agenda
;; https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :ensure t)

;; Code evaluation in org-mode
(use-package ob-fsharp
  :ensure t)

;; ======================================================
;; Latex + Beamer config
;; Beamer is a LaTeX package for writing presentations.
;; https://orgmode.org/worg/exporters/beamer/tutorial.html
;; sudo apt-get install -f texlive-latex-extra
(use-package ox-beamer)
;; https://www.aidanscannell.com/post/org-mode-resume/
(use-package ox-latex
  :init
  ;; code here will run immediately
  :config
  ;; code here will run after the package is loaded
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author...} to latex export
  ;; delete unwanted file extensions after latexMK
  (setq org-latex-logfiles-extensions
        (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))
  
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil)))

(use-package ox-extra
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package oc-biblatex)

;; GitHub Flavored Markdown
;; This package changes the default MD exporter to use the GitHub syntax.
;; https://github.com/larstvei/ox-gfm
(use-package ox-gfm
  :ensure t)
