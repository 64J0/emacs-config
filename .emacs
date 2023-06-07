;;; .emacs --- My custom Emacs setup -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my Emacs configuration file.

;;; Code:

;; ============================================
;; PACKAGE MANAGEMENT
;;
;; Next-generation, purely functional package manager for the Emacs hacker.
;; Repository: `https://github.com/radian-software/straight.el'
;;
;; Bootstrap script:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq package-enable-at-startup nil)

;; Displays the key bindings following your currently entered incomplete command
;; (a prefix) in a popup.
;;
;; Repository: `https://github.com/justbur/emacs-which-key'
(use-package which-key
  :straight t
  :custom (which-key-mode t))

;; super-save auto-saves your buffers, when certain events happen - e.g. you
;; switch between buffers, an Emacs frame loses focus, etc.
;;
;; Repository: `https://github.com/bbatsov/super-save'
(use-package super-save
  :straight t
  :custom
  (super-save-mode +1)
  (super-save-auto-save-when-idle t)
  (auto-save-default nil))

;; Helm is an Emacs framework for incremental completions and narrowing
;; selections.
;;
;; Repository: `https://github.com/emacs-helm/helm'
(use-package helm
  :straight t
  :bind (("C-x b" . helm-buffers-list)
         ("M-x" . helm-M-x)))

;; rainbow-delimiters is a "rainbow-parentheses"-like mode which highlight
;; delimiters such as parentheses, brackets or braces according to their depth.
;;
;; Repository: `https://github.com/Fanael/rainbow-delimiters'
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Minor mode for Emacs that deals with parens pairs and tries to be smart about
;; it.
;;
;; Repository: `https://github.com/Fuco1/smartparens'
(use-package smartparens
  :straight t
  :init (require 'smartparens-config)
  :custom
  (smartparens-global-mode t)
  (sp-show-pair-from-inside t)
  :custom-face
  (sp-show-pair-match-face ((t (:foreground "Purple" :background "Green")))))

;; Show directory tree on the lateral.
;;
;; Repository: `https://github.com/jaypei/emacs-neotree'
(use-package neotree
  :straight t
  :bind (("C-b" . neotree-toggle)))

;; Ivy: generic completion mechanism for Emacs
;; Counsel: collection of Ivy-enhanced versions of common Emacs commands
;; Swiper: Ivy-enhanced alternative to Isearch
;;
;; Repository: `https://github.com/abo-abo/swiper'
;; Book: https://oremacs.com/swiper/
(use-package counsel
  :straight t
  :bind (("C-x C-f" . counsel-find-file)
         ("C-s" . swiper-isearch))
  :custom (ivy-mode t))

;; Modern on-the-fly syntax checking extension for GNU Emacs.
;;
;; Repository: `https://github.com/flycheck/flycheck'
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;; COMplete ANYthing: modular in-buffer completion framework for Emacs.
;;
;; Repository: `https://github.com/company-mode/company-mode'
(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :commands (company-mode company-indent-or-complete-common)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t))

;; A company front-end with icons.
;;
;; Repository: `https://github.com/sebastiencs/company-box'
(use-package company-box
  :straight t
  :after (company)
  :hook (company-mode . company-box-mode))

;; Multile cursors to make our lifes easier.
;;                                        ;
;; Repository: `https://github.com/magnars/multiple-cursors.el'
(use-package multiple-cursors
  :straight t
  :bind (("C-S-l" . mc/mark-all-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; ============================================
;; Load external configuration
(message "Load external configuration")
(setq gajo--local-dir "~/Desktop/codes/emacs-config/")

(load-file (concat gajo--local-dir "src/theme.el"))
(load-file (concat gajo--local-dir "src/global.el"))
(load-file (concat gajo--local-dir "src/unfill-paragraph.el"))
(load-file (concat gajo--local-dir "src/org-mode.el"))
(load-file (concat gajo--local-dir "src/prog-mode.el"))
(load-file (concat gajo--local-dir "src/move-buffer.el"))

;;; .emacs ends here
