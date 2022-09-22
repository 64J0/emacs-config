;; -*- lexical-binding: t; -*-

;; Until Emacs 24.1 (June 2012), Elisp only had dynamically scoped variables, a
;; feature, mostly by accident, common to old lisp dialects. While dynamic scope
;; has some selective uses, it’s widely regarded as a mistake for local
;; variables, and virtually no other languages have adopted it.

(require 'cl)
(require 'cl-lib) ;; cl -> common lisp

;; =======================================================================
;; INITIAL CONFIG
;; Just used to set some default values to make Emacs look and behave the
;; way I want.
(setq user-full-name "Vinícius Gajo")
(global-linum-mode) ;; show the line number
(tool-bar-mode -1) ;; remove tool bar
(menu-bar-mode -1) ;; remove menu bar
(setq inhibit-startup-message t)  ;; remove startup message
(setq standard-indent 4) ;; default indent spaces
(setq auto-save-no-message t)
(setq column-number-mode t) ;; show coordinates (y, x)
(setq delete-selection-mode t) ;; delete text when selected and start typing
(setq system-time-locale "pt_BR.UTF-8") ;; set encode
(setq initial-buffer-choice "~/org/activities.org") ;; initial file 
;; https://stackoverflow.com/questions/12031830/what-are-file-and-file-and-how-can-i-get-rid-of-them
(setq make-backup-files nil) ;; avoid "~" files
(set-face-attribute 'default nil
		    :height 140
		    :family "DejaVu Sans Mono") ;; font size and family
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

;; =======================================================================
;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key [f5] 'find-alternate-file) ;; reload a file

;; =======================================================================
;; CUSTOM FUNCTIONS
;; Elisp functions I use to configure my Emacs.
(defun concat-deps-path (filename)
  "This function helps to avoid repeating the full path for the
   deps folder."
  (concat "~/org/deps/" filename))

;; https://www.emacswiki.org/emacs/UnfillParagraph
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; =======================================================================
;; CUSTOM VARIABLES
(setq gajo-org-srs-path "~/org/srs/deck-refile.org")
(setq gajo-org-notes-path "~/org/notes.org")
(setq gajo-org-refile-path "~/org/refile.org")
(setq gajo-org-agenda-path "~/org/agenda.org")
(setq gajo-org-email-path "~/org/email.org")
(setq gajo-org-todo-path "~/org/todo.org")
(setq gajo-org-work-path "~/org/work.org")
(setq gajo-org-habit-path "~/org/habit.org")
(setq gajo-org-meetings-path "~/org/meetings.org")

;; =======================================================================
;; PACKAGE REPOSITORIES
;; when error: M-x package-refresh-contents
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("elpagnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

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

;; Framework for incremental completions and narrowing selections.
;; https://emacs-helm.github.io/helm/
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list)))

;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

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

;; Convert buffer text and decorations to HTML
;; https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :ensure t)

;; Aesthetic tabs
;; https://github.com/ema2159/centaur-tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'over)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; Keybindings to comment line region and single line
(use-package undo-tree
  :ensure t
  :init
  (undo-tree-mode))

;; Displays help text (e.g. for buttons and menu items that you put the mouse on)
;; in a pop-up window.
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/tooltip.el
(use-package tooltip
  :config
  (tooltip-mode 0))

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

;; A GNU Emacs major mode for keeping notes, authoring documents,
;; computational notebooks, literate programming, maintaining to-do
;; lists, planning projects, and more.
;; https://orgmode.org/
;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
;; https://github.com/ebellani/Emacs.d/blob/master/init.el
(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :preface
  (setq org-export-backends '(moderncv md beamer ascii html latex odt org))
  :config
  ;; Required for PlantUML diagrams
  ;; From: https://plantuml.com/download
  (setq org-plantuml-jar-path
        (expand-file-name (concat-deps-path "plantuml-1.2021.16.jar")))
  (setq org-ditaa-jar-path
        (expand-file-name (concat-deps-path "ditaa0_9.jar")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql      . t)
     (dot      . t)
     (latex    . t)
     (shell    . t)
     (python   . t)
     (js       . t)
     (ocaml    . t)
     (java     . t)
     (scheme   . t)
     (plantuml . t)
     (sqlite   . t)
     (gnuplot  . t)
     (ditaa    . t)
     (C        . t)
     (ledger   . t)
     (org      . t)))
  (add-to-list
   'auto-mode-alist
   '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  (setq org-log-done t)
  (setq org-export-backends
	'(md gfm beamer ascii taskjuggler html latex odt org))
  (setq org-support-shift-select 'always)
  (setq org-directory "~/org")
  (setq org-default-notes-file gajo-org-notes-path)
  (setq org-refile-file-path gajo-org-refile-path)
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-refile-use-outline-path 'file)
  (setq org-babel-inline-result-wrap "%s")
  (setq org-habit-graph-column 60)
  (setq org-habit-following-days 0)
  (setq org-habit-preceding-days 14)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-use-fast-todo-selection t) ;; C-c C-t KEY
  (setq org-imenu-depth 6)
  (setq org-src-fontify-natively t)
  (setq org-use-sub-superscripts '{})
  (setq org-export-with-sub-superscripts '{})
  (setq org-duration-format '((special . h:mm)))
  (setq org-goto-interface 'outline-path-completion)
  ;; AGENDA
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-include-diary nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-sticky nil)
  (setq org-agenda-span 21)
  (setq org-agenda-files '(gajo-org-agenda-path))
  (setq org-latex-pdf-process (list "latexmk -silent -f -pdf %f"))
  (setq org-cite-export-processors '((latex biblatex)
                                     (moderncv basic)
                                     (t basic)))
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "red" :weight bold)
		("NEXT" :foreground "blue" :weight bold)
		("DONE" :foreground "forest green" :weight bold)
		("WAITING" :foreground "orange" :weight bold)
		("HOLD" :foreground "magenta" :weight bold)
		("CANCELLED" :foreground "forest green" :weight bold)
		("PHONE" :foreground "forest green" :weight bold)
		("MEETING" :foreground "forest green" :weight bold))))
  (setq org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
		("WAITING" ("WAITING" . t))
		("HOLD" ("WAITING") ("HOLD" . t))
		(done ("WAITING") ("HOLD"))
		("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
		("DONE" ("WAITING") ("CANCELLED") ("HOLD"))
		)))
  (setq org-tag-alist '((:startgroup)
                        ("noexport" . ?n)
                        ("export"   . ?e)
                        (:endgroup)))
  (setq org-refile-targets
        `((nil                     :maxlevel . 9)
          (org-agenda-files        :maxlevel . 2)
          (,gajo-org-srs-path      :maxlevel . 2)
          (,gajo-org-agenda-path   :maxlevel . 2)))
  (setq org-capture-templates
        `(("e" "Email [m4ue]" entry (file gajo-org-email-path)
           ,(concat "* TODO Process \"%a\"\n"
                    "SCHEDULED: %t\n"
                    ":LOGBOOK:\n"
                    "- State \"TODO\"     from\"\" %U   \\\\\n"
                    "  %^{Initial log} %?\n"
                    "  from %:from\n"
                    ":END:"))
          ("t" "todo" entry (file gajo-org-todo-path)
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %t\n"
                    ":PROPERTIES:\n"
                    ":BV:\n"
                    ":TC:\n"
                    ":RR-OE:\n"
                    ":EFF:\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    " - State \"TODO\"       from \"\"  %U  \\\\\n"
                    "  %^{Initial log} %?\n"
                    ":END:")
           :jump-to-captured t)
          ("w" "work reminder" entry (file gajo-org-work-path)
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: <%%(memq (calendar-day-of-week date) '(1 2 3 4 5))>%?\n"
                    ":PROPERTIES:\n"
                    ":work_reminder: t\n"
                    ":BV:\n"
                    ":TC:\n"
                    ":RR-OE:\n"
                    ":EFF:\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    "- Initial note taken on %U \\\n"
                    "%^{Initial note}\n"
                    ":END:\n"))
          ("h" "habit" entry (file gajo-org-habit-path)
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %(org-insert-time-stamp nil nil nil nil nil \" +1w\")%?\n"
                    ":PROPERTIES:\n"
                    ":style: habit\n"
                    ":BV:\n"
                    ":TC:\n"
                    ":RR-OE:\n"
                    ":EFF:\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    "- State \"TODO\"       from \"\"  %U  \\\\\n"
                    "%^{Initial log}\n"
                    ":END:\n"))
          ("m" "meeting log" entry (file ,gajo-org-meetings-path)
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
                    "%?\n"))))
  )

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
  )

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

;; Highlight uncommited changes on the left side of the window
;; area known as the "gutter"
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; Supercharge your org daily/weekly/agenda
;; https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :ensure t)

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
        read-process-output-max (* 1024 1024) ;; 1mb
        lsp-idle-delay 1.000
        lsp-log-io nil) ; if set to true can cause a performance hit
  ;; UI
  (setq lsp-headerline-breadcrumb-enable t)
  ;; F# ---------------------------------
  ;; (add-hook 'before-save-hook #'(lambda () (when (eq major-mode 'fsharp-mode)
  ;;                                            (lsp-format-buffer))))
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

;; This package contains all the higher level UI modules of lsp-mode,
;; like flycheck support and code lenses.
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
  :config
  (use-package flymake-flycheck
    :ensure t))

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

;; https://docs.projectile.mx/projectile/installation.html
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

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
(use-package magit
  :ensure t)

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
   (setq compile-command "dotnet watch run")
   (setq inferior-fsharp-program "dotnet fsi")
   (add-hook 'inferior-fsharp-mode-hook 'turn-on-comint-history)
   (add-hook 'fsharp-mode-hook 'highlight-indentation-mode))

;; Code evaluation in org-mode
(use-package ob-fsharp
  :ensure t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(typescript-mode lsp-ivy company-box dired python-mode projectile lsp-treemacs undo-tree terraform-mode yaml-mode dockerfile-mode elpy json-mode ob-fsharp fsharp-mode company-quickhelp lsp-ui flymake-flycheck flycheck magit org-super-agenda diff-hl org-contrib org-superstar org-drill org-roam counsel company centaur-tabs htmlize neotree rainbow-delimiters helm-lsp helm which-key dracula-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
