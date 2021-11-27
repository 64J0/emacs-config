;; https://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs
;; https://github.com/ebellani/Emacs.d/blob/master/init.el
(require 'cl)
(require 'cl-lib) ;; cl -> common lisp

;; INITIAL CONFIG
(setq inhibit-startup-message t
      standard-indent 4
      auto-save-no-message t) ;; remove startup message
(menu-bar-mode -1) ;; remove menu bar
(global-linum-mode t) ;; show the line number
(set-face-attribute 'default nil
		    :height 140
		    :family "DejaVu Sans Mono") ;; font-size
(setq delete-selection-mode t) ;; delete text when selected and start typing
(setq system-time-locale "pt_BR.UTF-8")
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil
	      fill-column 80)

(defun buffer/insert-filename ()
  "Insert file name of current buffer at current point"
  (interactive)
  (insert (buffer-file-name (current-buffer))))

;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)

;; ;; DEBUG PURPOSES
;; (if init-file-debug
;;     (setq use-package-verbose t
;; 	  use-package-expand-minimally nil
;; 	  use-package-compute-statistics t
;; 	  debug-on-error t)
;;   (setq use-package-verbose nil
;; 	use-package-expand-minimally t))

;; ;; PATH SETUP
;; (defcustom my/path-aliases
;;   (list :emacs  "~/.emacs.d"
;; 	:srs    "~/.emacs.d"
;; 	:work   "~/.emacs.d"
;; 	:agenda "~/.emacs.d")
;;   "Location of my paths for ease of usage. Customize for each env
;;    if needed.")

;; (defcustom my/path (dir $optional subpath)
;;   "Build a path name. See https://github.com/arecker/emacs.d"
;;   (let ((dir (file-name-as-directory
;; 	      (cl-getf my/path-aliases dir
;; 		       (format "~/%s" dir))))
;; 	(subpath (or subpath "")))
;;     (concat dir subpath)))

;; (defcustom main-agenda (my/path :emacs "agenda.org")
;;   "This is used to store quickly todo items without refiling.")

;; (add-to-list 'load-path (my/path :emacs "lib"))

;; ;; add the custom file inside the emacs folder

;; (let ((custom-file-path (my/path :emacs "custom.el")))
;;   (if (file-readable-p custom-file-path)
;;       (progn
;; 	(setq custom-file custom-file-path)
;; 	(load custom-file))
;;     (warn "Custom file not found at expected path %s" custom-file-path)))

;; PACKAGE REPOSITORIES
;; when error: M-x package-refresh-contents
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org"  . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; automatically download and load other packages from the emacs
;; package databases when it detects they're missing.
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(load-theme 'dracula t)

;; COMplete ANYthing
;; Could give wrong completions (orgmode)
;; http://company-mode.github.io/
(use-package company
  :ensure t
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setf company-idle-delay 0
        company-selection-wrap-around t)
  :hook (after-init . global-company-mode))

;; Puts angry red squiggles on the screen when I do something stupid.
;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

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

;; https://github.com/emacs-lsp/helm-lsp
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

;; Show directory tree on the lateral
;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :ensure t
  :bind (("C-b" . 'neotree-toggle)))

;; Convert buffer text and decorations to HTML
;; https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :ensure t)

;; A GNU Emacs major mode for keeping notes, authoring documents,
;; computational notebooks, literate programming, maintaining to-do
;; lists, planning projects, and more.
;; https://orgmode.org/
(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :preface
  (setq org-export-backends '(moderncv md gfm beamer ascii taskjuggler html latex odt org))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (dot     . t)
     (latex   . t)
     (shell   . t)
     (python  . t)
     (js      . t)
     (ditaa   . t)
     (ocaml   . t)
     (java    . t)
     (scheme  . t)
     (plantuml . t)
     (ditaa   . t)
     (sqlite  . t)
     (gnuplot . t)
     (ditaa  . t)
     (C      . t)
     (ledger . t)
     (org    . t)))
  (add-to-list
   'auto-mode-alist
   '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
        org-todo-keyword-faces
	(quote (("TODO" :foreground "red" :weight bold)
		("NEXT" :foreground "blue" :weight bold)
		("DONE" :foreground "forest green" :weight bold)
		("WAITING" :foreground "orange" :weight bold)
		("HOLD" :foreground "magenta" :weight bold)
		("CANCELLED" :foreground "forest green" :weight bold)
		("PHONE" :foreground "forest green" :weight bold)
		("MEETING" :foreground "forest green" :weight bold))
	       )
        org-use-fast-todo-selection t ;; C-c C-t KEY
        org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
		("WAITING" ("WAITING" . t))
		("HOLD" ("WAITING") ("HOLD" . t))
		(done ("WAITING") ("HOLD"))
		("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
		("DONE" ("WAITING") ("CANCELLED") ("HOLD"))
		))
        org-log-done t
        org-export-backends
	'(md gfm beamer ascii taskjuggler html latex odt org)
        org-support-shift-select
	'always
        org-directory "~/org"
        org-default-notes-file "~/org/refile.org"
        org-capture-templates
	(quote (("t" "todo" entry (file "~/org/refile.org")
		 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("r" "respond" entry (file "~/org/refile.org")
		 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
		("n" "note" entry (file "~/org/refile.org")
		 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		("j" "Journal" entry (file+datetree "~/org/refile.org")
		 "* %?\n%U\n" :clock-in t :clock-resume t)
		("w" "org-protocol" entry (file "~/org/refile.org")
		 "* TODO Review %c\n%U\n" :immediate-finish t)
		("m" "Meeting" entry (file "~/org/refile.org")
		 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
		("p" "Phone call" entry (file "~/org/refile.org")
		 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
		("h" "Habit" entry (file "~/org/refile.org")
		 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
		)))
  ;; Bellani config area
  (setq org-refile-file-path "~/org/refile.org"
        org-refile-allow-creating-parent-nodes 'confirm
        org-babel-inline-result-wrap "%s"
        org-habit-graph-column 60
        org-habit-following-days 0
        org-habit-preceding-days 14
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-tag-alist '((:startgroup)
                        ("noexport" . ?n)
                        ("export" . ?e)
                        (:endgroup))
        org-refile-targets
        `((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 2)
          (,"~/org/srs/deck.org" :maxlevel . 2)
          (,"~/org/work/meetings.org" :maxlevel . 2))
        org-capture-templates
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
                    "%?\n")))
        org-todo-keywords
        '((sequence "TODO(t@/!)" "|" "DONE(d@/!)")
          (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
          (sequence "REPEAT(r@/!)"))
        org-imenu-depth 6
        org-src-fontify-natively t
        org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{}
        org-babel-default-header-args
        (cons '(:noweb . "yes")
              (assq-delete-all :noweb org-babel-default-header-args))
        org-babel-default-header-args
        (cons '(:tangle . "yes")
              (assq-delete-all :tangle org-babel-default-header-args))
        org-babel-default-header-args
        (cons '(:comments . "link")
              (assq-delete-all :comments org-babel-default-header-args))
        org-duration-format '((special . h:mm))
        org-goto-interface 'outline-path-completion
        ;; agenda stuff copied from
        ;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-block-separator nil
        org-agenda-include-diary nil
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t
        ;; allows multiple agenda views to coexist
        org-agenda-sticky nil ;; setting it to t breaks capture from agenda, for now
        org-agenda-span 'day
        org-plantuml-jar-path "/home/user/bin/plantuml.jar"
        org-latex-pdf-process (list "latexmk -silent -f -pdf %f")
        org-cite-export-processors '((latex biblatex)
                                     (moderncv basic)
                                     (t basic))
        )
  )

;; Taskjuggler is a project planning software which uses a plain text file for the
;; definition of tasks which is then processed to create the schedule.
;; https://www.skamphausen.de/cgi-bin/ska/taskjuggler-mode
;; (use-package taskjuggler-mode
;;   :ensure t)

;; Displays help text (e.g. for buttons and menu items that you put the mouse on)
;; in a pop-up window.
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/tooltip.el
(use-package tooltip
  :config
  (tooltip-mode 0))

;; Spaced repetition algorithm to conduct interactive "drill sessions",
;; using org files as sources of facts to be memorised.
;; https://orgmode.org/worg/org-contrib/org-drill.html
(use-package org-drill
  :ensure t
  :commands (org-drill))

;; Supercharge your org daily/weekly/agenda
;; https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :ensure t)

;; ======================================================
;; F# CONFIG

;; This package gives you a set of key combinations to perform dotnet
;; CLI tasks within your .NET Core projects
;; https://github.com/julienXX/dotnet.el
(use-package dotnet
  :ensure t
  :hook (fsharp-mode . dotnet-mode))

;; An Emacs LSP client that stays out of your way
;; https://github.com/joaotavora/eglot
(use-package eglot
  :ensure t
  :after company)

;; Provides support for the F# language in Emacs
;; https://github.com/fsharp/emacs-fsharp-mode
(use-package fsharp-mode
  :ensure t
  :after company
  :mode (("\\.fs$" .  fsharp-mode)
	 ("\\.fsx$" .  fsharp-mode))
  :config
  (setq-default fsharp-indent-offset 4)
  (setq inferior-fsharp-program "dotnet fsi")
  :hook (fsharp-mode . highlight-indentation-mode))

(use-package eglot-fsharp
  :ensure t
  :after fsharp-mode
  :init
  (add-hook 'inferior-fsharp-mode-hook 'turn-on-comint-history))

;; Language Server Protocol Support for Emacs
;; Aims to provide IDE-like experience by providing optional integration
;; with the most popular Emacs packages like comapny, flycheck and
;; projectile.
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :hook (fsharp-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

;; This package contains all the higher level UI modules of lsp-mode,
;; like flycheck support and code lenses.
;; https://emacs-lsp.github.io/lsp-ui/#intro
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Is a complete text-based user interface to Git.
;; https://magit.vc/
(use-package magit
  :ensure t)

;; Replacement of DocView for PDF files.
;; https://github.com/politza/pdf-tools
(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))

;; ======================================================
;; DEVSECOPS

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

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
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

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

;; ======================================================
;; AUTOMATIC GENERATED
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (json-mode yaml-mode haskell-mode slime-company kubernetes dockerfile-mode flycheck org-super-agenda helm-lsp lsp-ui lsp-mode company magit org-drill org-plus-contrib dotnet eglot-fsharp org-pdfview pdf-tools highlight-indent-guides htmlize fsharp-mode neotree auto-complete dracula-theme helm try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
