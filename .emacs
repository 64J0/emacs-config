;; https://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs
;; https://github.com/ebellani/Emacs.d/blob/master/init.el
(require 'cl)
(require 'cl-lib) ;; cl -> common lisp

;; INITIAL CONFIG
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
(setq make-backup-files nil) ;; avoid ~ files

(set-face-attribute 'default nil
		    :height 140
		    :family "DejaVu Sans Mono") ;; font size and family

(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "C-/") 'comment-line)

;; PACKAGE REPOSITORIES
;; when error: M-x package-refresh-contents
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org"  . "https://orgmode.org/elpa/"))
(package-initialize)

;; automatically download and load other packages from the emacs
;; package databases when it detects they're missing.
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

;; Displays the key bindings following your currently entered incomplete
;; command (a prefix) in a popup.
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Framework for incremental completions and narrowing selections.
;; https://emacs-helm.github.io/helm/
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list)))

;; lsp -> language server protocol
;; https://github.com/emacs-lsp/helm-lsp
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

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

;; COMplete ANYthing
;; Could give wrong completions (orgmode)
;; http://company-mode.github.io/
(use-package company
  :ensure t
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setf company-idle-delay 1
        company-selection-wrap-around t)
  :hook (after-init . global-company-mode))

;; https://github.com/alhassy/emacs.d#quickly-pop-up-a-terminal-run-a-command-close-it-and-zsh
(use-package shell-pop
  :ensure t
  :custom
    ;; This binding toggles popping up a shell, or moving cursour to the shell pop-up.
    (shell-pop-universal-key "C-t")
    ;; Percentage for shell-buffer window size.
    (shell-pop-window-size 30)
    ;; Position of the popped buffer: top, bottom, left, right, full.
    (shell-pop-window-position "bottom")
    ;; Please use an awesome shell.
    (shell-pop-term-shell "/bin/bash"))

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
        (expand-file-name "~/Desktop/codes/emacs-config/deps/plantuml-1.2021.16.jar"))
  (setq org-ditaa-jar-path
        (expand-file-name "~/Desktop/codes/emacs-config/deps/ditaa0_9.jar"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql      . t)
     (dot      . t)
     (latex    . t)
     (shell    . t)
     (python   . t)
     (js       . t)
     (ditaa    . t)
     (ocaml    . t)
     (java     . t)
     (scheme   . t)
     (plantuml . t)
     (ditaa    . t)
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
  (setq org-default-notes-file "~/org/notes-refile.org")
  (setq org-refile-file-path "~/org/refile.org")
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
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-include-diary nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-sticky nil)
  (setq org-agenda-span 'day)
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
        `((nil                          :maxlevel . 9)
          (org-agenda-files             :maxlevel . 2)
          (,"~/org/srs/deck.org"        :maxlevel . 2)
          (,"~/org/agenda/meetings.org" :maxlevel . 2)))
  (setq org-capture-templates
        `(("e" "Email [m4ue]" entry (file main-agenda)
           ,(concat "* TODO Process \"%a\"\n"
                    "SCHEDULED: %t\n"
                    ":LOGBOOK:\n"
                    "- State \"TODO\"     from\"\" %U   \\\\\n"
                    "  %^{Initial log} %?\n"
                    "  from %:from\n"
                    ":END:"))
          ("t" "todo" entry
           (file main-agenda)
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
                    ":END:"))
          ("w" "work reminder" entry
           (file main-agenda)
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
          ("h" "habit" entry
           (file main-agenda)
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
          ("m" "meeting log" entry
           (file ,"~/org/work/meetings.org")
           ,(concat "* %^{Title}\n"
                    "** Context\n"
                    "%^{Context}\n"
                    "** Goal\n"
                    "%^{Goal}\n"
                    "** Agenda\n"
                    "%^{Agenda}\n"
                    "** Ata\n"
                    "%^{Minutes})\n"))
          ("d" "Drill card with answer" entry
           (file ,"~/org/srs/deck.org")
           ,(concat "* Item           :drill:\n"
                    "%^{Question}\n"
                    "** Answer\n"
                    "%^{Answer}\n"))
          ("z" "Drill" entry
           (file ,"~/org/srs/deck.org")
           ,(concat "* Item           :drill:\n"
                    "%?\n"))
          ("x" "Drill cloze 2" entry
           (file ,"~/org/srs/deck.org")
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
  (org-roam-directory "~/Desktop/codes/emacs-config/RoamNotes")
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

;; https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :ensure t
  :init
  (add-hook
   'org-mode-hook (lambda () (org-superstar-mode 1))))

;; https://github.com/PillFall/languagetool.el
;; https://dev.languagetool.org/java-api
(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "~/LanguageTool-5.6/languagetool-commandline.jar"
        languagetool-server-command "~/LanguageTool-5.6/languagetool-server.jar"))

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

;; Replacement of DocView for PDF files.
;; https://github.com/politza/pdf-tools
(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))

;; Is a complete text-based user interface to Git.
;; https://magit.vc/
(use-package magit
  :ensure t)

;; ======================================================
;; Latex + Beamer config
;; Beamer is a LaTeX package for writing presentations.
;; https://orgmode.org/worg/exporters/beamer/tutorial.html
;; sudo apt-get install -f texlive-latex-extra
(require 'ox-beamer)
(require 'ox-latex)

;; https://github.com/bdarcus/citar
(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography '("~/Desktop/codes/emacs-config/bib/references.bib")))

;; ======================================================
;; F# CONFIG
;; Got this configuration from Magueta's config
;; https://github.com/MMagueta/MageMacs/blob/macintosh/init.el

;; Static analysis
;; https://github.com/emacs-elsa/Elsa
;; (use-package elsa
;;   :ensure t
;;   :hook
;;   (emacs-lisp-mode . (lambda () (flycheck-elsa-setup)))
;;   :config
;;   (use-package flycheck-elsa
;;     :ensure t
;;     :hook
;;     ((emacs-lisp-mode . (lambda () (flycheck-mode)))
;;      (emacs-lisp-mode . (lambda () (flymake-mode))))))

;; Puts angry red squiggles on the screen when I do something stupid.
;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :ensure t
  :config
  (use-package flymake-flycheck
    :ensure t))

;; Language Server Protocol Support for Emacs
;; Aims to provide IDE-like experience by providing optional integration
;; with the most popular Emacs packages like comapny, flycheck and
;; projectile.
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (fsharp-mode . lsp-lens-mode)
  :init
  (add-hook 'before-save-hook #'(lambda () (when (eq major-mode 'fsharp-mode)
                                             (lsp-format-buffer))))
  :config
  (use-package lsp-treemacs
    :ensure t))

;; Emacs client/library for Debug Adapter Protocol is a wire protocol for
;; communication between client and Debug Server. It's similar to the LSP
;; but provides integration with debug server.
;; https://github.com/emacs-lsp/dap-mode
;; (use-package dap-mode
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   ;; Enabling only some features
;;   (setq dap-auto-configure-features '(sessions locals controls tooltip))
;;   (dap-mode 1)
;;   ;; The modes below are optional
;;   (dap-ui-mode 1)
;;   ;; enables mouse hover support
;;   (dap-tooltip-mode 1)
;;   ;; use tooltips for mouse hover
;;   ;; if it is not enabled `dap-mode' will use the minibuffer.
;;   (tooltip-mode 1)
;;   ;; displays floating panel with debug buttons
;;   ;; requies emacs 26+
;;   (dap-ui-controls-mode 1)
;;   (add-hook 'dap-stopped-hook
;;             (lambda (arg) (call-interactively #'dap-hydra)))
;;   :hook
;;   (lsp-mode . dap-mode)
;;   (lsp-mode . dap-ui-mode))

;; (defun magueta/lsp-ui-doc-toggle ()
;;   "For some reason it is required to do at least once a call to lsp-ui-doc-show in order for this to work."
;;   (interactive)
;;   (let ((frame (lsp-ui-doc--get-frame)))
;;     (cond ((frame-visible-p frame) (lsp-ui-doc-hide))
;; 	  (t (lsp-ui-doc-show)))))

;; This package contains all the higher level UI modules of lsp-mode,
;; like flycheck support and code lenses.
;; https://emacs-lsp.github.io/lsp-ui/#intro
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-diagnostic-max-lines 7))

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

;; Provides support for the F# language in Emacs
;; https://github.com/fsharp/emacs-fsharp-mode
(use-package fsharp-mode
   :ensure t
   :mode (("\\.fs$"  .  fsharp-mode)
	  ("\\.fsx$" .  fsharp-mode)
	  ("\\.fsi$" .  fsharp-mode))
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
   (add-hook 'inferior-fsharp-mode-hook 'turn-on-comint-history))

(use-package eshell-syntax-highlighting
  :ensure t
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; This package gives you a set of key combinations to perform dotnet
;; CLI tasks within your .NET Core projects
;; https://github.com/julienXX/dotnet.el
;; (use-package dotnet
;;   :ensure t
;;   :hook (fsharp-mode . dotnet-mode))

;; Code evaluation in org-mode
(use-package ob-fsharp
  :ensure t)

;; An Emacs LSP client that stays out of your way
;; https://github.com/joaotavora/eglot
(use-package eglot
  :ensure t
  :after company
  :config
  (use-package eglot-fsharp
    :ensure t
    :after fsharp-mode
    :config
    (setq toggle-debug-on-error t)
    (setq lsp-print-io t)))

;; ======================================================
;; DEVSECOPS

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; Python code
;; to fix problems: https://www.higithub.com/jorgenschaefer/issue/elpy/1936
;; M-x elpy-rpc-reinstall-virtualenv
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; Pretty syntax highlight for editing Dockerfiles.
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode ("\\Dockerfile\\'" "\\.dockerfile\\'"))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))
(global-set-key (kbd "C-c K") 'kubernetes-overview)

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'"))

(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save))

;; https://github.com/emacsorphanage/terraform-mode
(use-package terraform-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  (setq terraform-indent-level 2))

;; ======================================================
;; OTHER LANGS
;; C
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook (lambda ()
                         (setq indent-tabs-mode t)
                         (setq show-trailing-whitespace t)
                         (c-set-style "linux-tabs-only")))

;; COMMON LISP
(use-package slime
  :ensure t
  :defer t
  :config (setq inferior-lisp-program (executable-find "sbcl")))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

;; HASKELL
(use-package haskell-mode
  :ensure t
  :defer t
  :mode "\\.hs\\'")

;; Keybindings to comment line region and single line
(use-package undo-tree
  :ensure t
  :init
  (undo-tree-mode))

;; ======================================================
;; AUTOMATIC GENERATED
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '())
 '(package-selected-packages
   '(counsel shell-pop languagetool highlight-indentation-mode company-quickhelp eshell-syntax-highlighting all-the-icons flymake-flycheck elsa ox-latex ox-beamer org-superstar terraform-mode rainbow-delimiters lsp-grammarly diff-hl diff-hl-mode ob-fsharp org-roam centaur-tabs ox-publish go-mode json-mode yaml-mode haskell-mode slime-company kubernetes dockerfile-mode flycheck org-super-agenda helm-lsp lsp-ui lsp-mode company magit org-drill org-plus-contrib dotnet eglot-fsharp org-pdfview pdf-tools highlight-indent-guides htmlize fsharp-mode neotree auto-complete dracula-theme helm try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
