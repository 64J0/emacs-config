;; https://a-nickels-worth.blogspot.com/2007/11/effective-emacs.html
;; http://sites.google.com/site/steveyegge2/effective-emacs
;; https://github.com/ebellani/Emacs.d/blob/master/init.el
(require 'cl-lib) ;; cl -> common lisp

;; INITIAL CONFIG
(setq inhibit-startup-message t
      inhibit-startup-screen nil
      standard-indent 4
      auto-save-no-message t) ;; remove startup message
(menu-bar-mode -1) ;; remove menu bar
(global-linum-mode t) ;; show the line number
(set-face-attribute 'default nil
		    :height 140
		    :family "DejaVu Sans Mono") ;; font-size

;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)

;; DEBUG PURPOSES
(if init-file-debug
    (setq use-package-verbose t
	  use-package-expand-minimally nil
	  use-package-compute-statistics t
	  debug-on-error t)
  (setq use-package-verbose nil
	use-package-expand-minimally t))

;; PATH SETUP
(defcustom my/path-aliases
  (list :emacs  "~/.emacs.d"
	:srs    "~/.emacs.d"
	:work   "~/.emacs.d"
	:agenda "~/.emacs.d")
  "Location of my paths for ease of usage. Customize for each env
   if needed.")

(defcustom my/path (dir $optional subpath)
  "Build a path name. See https://github.com/arecker/emacs.d"
  (let ((dir (file-name-as-directory
	      (cl-getf my/path-aliases dir
		       (format "~/%s" dir))))
	(subpath (or subpath "")))
    (concat dir subpath)))

(defcustom main-agenda (my/path :emacs "agenda.org")
  "This is used to store quickly todo items without refiling.")

(add-to-list 'load-path (my/path :emacs "lib"))

;; add the custom file inside the emacs folder

(let ((custom-file-path (my/path :emacs "custom.el")))
  (if (file-readable-p custom-file-path)
      (progn
	(setq custom-file custom-file-path)
	(load custom-file))
    (warn "Custom file not found at expected path %s" custom-file-path)))

;; things that I don't know how to do with use-package

(setq system-time-locale "pt_BR.UTF-8")

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
	      fill-column 80)

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
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)))
  (add-to-list
   'auto-mode-alist
   '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
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
		("MEETING" :foreground "forest green" :weight bold))
	       ))
  (setq org-use-fast-todo-selection t) ;; C-c C-t KEY
  (setq org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
		("WAITING" ("WAITING" . t))
		("HOLD" ("WAITING") ("HOLD" . t))
		(done ("WAITING") ("HOLD"))
		("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
		("DONE" ("WAITING") ("CANCELLED") ("HOLD"))
		)))
  (setq org-log-done t)
  (setq	org-export-backends
	'(md gfm beamer ascii taskjuggler html latex odt org))
  (setq org-support-shift-select
	'always)
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/refile.org")
  (setq org-capture-templates
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

  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
				   (org-agenda-files :maxlevel .9))))

  ;; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path t)

  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  ;; Use IDO for both buffer and file completion and ido-everywhere to t
  (setq org-completion-use-ido t)
  (setq ido-everywhere t)
  (setq ido-max-directory-size 100000)
  (ido-mode (quote both))

  ;; CHAPTER 8
  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)
  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
	(quote (("N" "Notes" tags "NOTE"
		 ((org-agenda--insert-overriding-header "Notes")
		  (org-tags-match-list-sublevels t)))
		("h" "Habits" tags-todo "STYLE=\"habit\""
		 ((org-agenda--insert-overriding-header "Habits")
		  (org-agenda-sorting-strategy
		   '(todo-state-down effort-up category-keep))))
		(" " "Agenda"
		 ((agenda "" nil)
		  (tags "REFILE"
			((org-agenda--insert-overriding-header "Tasks to Refile")
			 (org-tags-match-list-sublevels nil)))
		  (tags-todo "-CANCELLED/!"
			     ((org-agenda--insert-overriding-header "Stuck Projects")
			      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
			      (org-agenda-sorting-strategy
			       '(category-keep))))
		  (tags-todo "-HOLD-CANCELLED/!"
			     ((org-agenda--insert-overriding-header "Projects")
			      (org-agenda-skip-function 'bh/skip-non-projects)
			      (org-tags-match-list-sublevels 'indented)
			      (org-agenda-sorting-strategy
			       '(category-keep))))
		  (tags-todo "-CANCELLED/!NEXT"
			     ((org-agenda--insert-overriding-header (concat "Project Next Tasks"
									    (if bh/hide-scheduled-and-waiting-next-tasks
										""
									      " (including WAITING and SCHEDULED tasks)")))
			      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
			      (org-tags-match-list-sublevels t)
			      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-sorting-strategy
			       '(todo-state-down effort-up category-keep))))
		  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
			     ((org-agenda-overriding-header (concat "Standalone Tasks"
								    (if bh/hide-scheduled-and-waiting-next-tasks
									""
								      " (including WAITING and SCHEDULED tasks)")))
			      (org-agenda-skip-function 'bh/skip-project-tasks)
			      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-sorting-strategy
			       '(category-keep))))
		  (tags-todo "-CANCELLED+WAITING|HOLD/!"
			     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
								    (if bh/hide-scheduled-and-waiting-next-tasks
									""
								      " (including WAITING and SCHEDULED tasks)")))
			      (org-agenda-skip-function 'bh/skip-non-tasks)
			      (org-tags-match-list-sublevels nil)
			      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
		  (tags "-REFILE/"
			((org-agenda-overriding-header "Tasks to Archive")
			 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
			 (org-tags-match-list-sublevels nil))))
		 nil))))
  (defun bh/org-auto-exclude-function (tag)
    "Automatic task exclusion in the agenda with / RET"
    (and (cond
	  ((string= tag "hold")
	   t)
	  ((string= tag "farm")
	   t))
	 (concat "-" tag)))
  (setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)
  (setq org-export-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  )

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
  :config
  (setq-default fsharp-indent-offset 4)
  (setq inferior-fsharp-program "dotnet fsi --readline-")
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-super-agenda helm-lsp lsp-ui lsp-mode company magit org-drill org-plus-contrib dotnet eglot-fsharp org-pdfview pdf-tools highlight-indent-guides htmlize fsharp-mode neotree auto-complete dracula-theme helm try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
