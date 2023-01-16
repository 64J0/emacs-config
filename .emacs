;; -*- lexical-binding: t; -*-

;; Based on: https://github.com/bbatsov/prelude

(require 'cl-lib) ;; cl -> common lisp

;; =======================================================================
;; PACKAGE MANAGEMENT
;; when error: M-x package-refresh-contents
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
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

;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

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

;; ============================================
;; Load external configuration
(setq gajo-dir "~/Desktop/codes/emacs-config/")
(load-file (concat gajo-dir "src/global.el"))
(load-file (concat gajo-dir "src/unfill-paragraph.el"))
(load-file (concat gajo-dir "src/my-org.el"))
(load-file (concat gajo-dir "src/programming.el"))
