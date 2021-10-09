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

;; adding other package repositories
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
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))
(use-package neotree
  :ensure t
  :bind (("C-\\" . 'neotree-toggle)))
(use-package htmlize
  :ensure t)

;; self performance
(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-log-done t)
  (setq org-export-backends
	'(md gfm beamer ascii taskjuggler html latex odt org))
  (setq org-support-shift-select 'always))
(use-package org-drill
  :ensure t
  :commands (org-drill))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))
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
