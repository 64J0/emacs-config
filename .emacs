(setq inhibit-startup-message t)
(menu-bar-mode -1)
(global-linum-mode t)
(set-face-attribute 'default nil :height 140)

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org"  . "https://orgmode.org/elpa/"))

(package-initialize)

;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list)))

(load-theme 'dracula t)

(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-log-done t)
  (setq org-export-backends '(md gfm beamer ascii taskjuggler html latex odt org))
  (setq org-support-shift-select 'always))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(use-package htmlize
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq-default highlight-indent-guides-method 'character)
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

(use-package neotree
  :ensure t
  :bind (("C-\\" . 'neotree-toggle)))

;; f# config
(use-package eglot
  :ensure t)

(use-package fsharp-mode
  :ensure t
  :mode ("\\.fs\\'" . fsharp-mode)
  :config
  (setq-default fsharp-indent-offset 4)
  (setq inferior-fsharp-program "dotnet fsi")
  :init
  (add-hook 'fsharp-mode-hook 'highlight-indentation-mode))

(use-package eglot-fsharp
  :ensure t
  :init
  (add-hook 'inferior-fsharp-mode-hook 'turn-on-comint-history))

(use-package dotnet
  :ensure t)

(use-package org-drill
  :ensure t
  :commands (org-drill))

;; for pdf
(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))

;; browsers - not used yet
(defun external-browser! ()
  "Makes an external browser the `browse-url-browser-function'."
  (interactive)
  (setq browse-url-browser-function 'browse-url-chrome))

(defun internal-browser! ()
  "Makes an internal browser the `browse-url-browser-function'."
  (interactive)
  (setq browse-url-browser-function 'eww-browse-url))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-drill org-plus-contrib dotnet eglot-fsharp org-pdfview pdf-tools highlight-indent-guides htmlize fsharp-mode neotree auto-complete dracula-theme helm try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
