;; initial configuration
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(global-linum-mode t)
(set-face-attribute 'default nil :height 140)

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

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

;; automatically download and install other packages from the emacs
;; package databases when it detects they're missing.
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; general use
(load-theme 'dracula t)
(use-package company
  :ensure t
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setf company-idle-delay 0
        company-selection-wrap-around t)
  :hook (after-init . global-company-mode))
(use-package try
  :ensure t)
(use-package which-key
  :ensure t
  :config (which-key-mode))
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list)))
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)
;(use-package auto-complete
;  :ensure t
;  :init
;  (progn
;    (ac-config-default)
;    (global-auto-complete-mode t)))
(use-package neotree
  :ensure t
  :bind (("C-b" . 'neotree-toggle)))
(use-package htmlize
  :ensure t)

;; self performance
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
  )
	
(use-package org-drill
  :ensure t
  :commands (org-drill))
(use-package org-super-agenda
  :ensure t)

;; f# config
(use-package dotnet
  :ensure t)
(use-package eglot
  :ensure t
  :after company)
(use-package eglot-fsharp
  :ensure t
  :after fsharp-mode
  :init
  (add-hook 'inferior-fsharp-mode-hook 'turn-on-comint-history))
(use-package fsharp-mode
  :ensure t
  :after company
  :mode ("\\.fsx?\\'" . fsharp-mode)
  :config
  (setq-default fsharp-indent-offset 4)
  (setq inferior-fsharp-program "dotnet fsi")
  :init
  (add-hook 'fsharp-mode-hook 'highlight-indentation-mode))
(use-package lsp-mode
  :ensure t
  :hook (fsharp-mode . lsp-deferred)
  :commands (lsp lsp-deferred))
;; https://emacs-lsp.github.io/lsp-ui/#intro
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; c programming language
;; not done yet

;; git tool
(use-package magit
  :ensure t)

;; for pdf
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
