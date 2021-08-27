(setq inhibit-startup-message t)
(menu-bar-mode -1)
(global-linum-mode t)
(set-face-attribute 'default nil :height 140)

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org"  . "https://orgmode.org/elpa/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package helm
  :ensure t)

(use-package org
  :ensure t)

;; HELM CONFIG
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(load-theme 'dracula t)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)
(setq org-export-backends '(md gfm beamer ascii taskjuggler html latex odt org))

;; helm
;; org
;; f# config
;; dracula-theme

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (dracula-theme helm try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
