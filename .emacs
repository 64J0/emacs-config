;; -*- lexical-binding: t; -*-

;; Based on: https://github.com/bbatsov/prelude

(require 'cl-lib) ;; cl -> common lisp

;; =======================================================================
;; PACKAGE MANAGEMENT
(setq package-enable-at-startup nil)
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

;; ============================================
;; Load external configuration
(setq gajo-dir "~/Desktop/codes/emacs-config/")
(load-file (concat gajo-dir "src/global.el"))
(load-file (concat gajo-dir "src/unfill-paragraph.el"))
(load-file (concat gajo-dir "src/org-mode.el"))
(load-file (concat gajo-dir "src/prog-mode.el"))

;; ============================================
;; EMACS THEME
;; https://www.reddit.com/r/emacs/comments/j7eruf/favorite_light_themes/
(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-opera-light t))
;; (load-theme 'modus-operandi t)

;; ============================================
;; GENERAL USAGE
;; Displays the key bindings following your currently entered incomplete
;; command (a prefix) in a popup.
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package super-save
  :straight t
  :config
  (super-save-mode +1)
  :init
  (setq super-save-auto-save-when-idle t))

;; Framework for incremental completions and narrowing selections.
;; https://emacs-helm.github.io/helm/
(use-package helm
  :straight t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list)))

;; Highlight delimiters such as parentheses, brackets or braces
;; according to their depth
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Show directory tree on the lateral
;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :straight t
  :bind (("C-b" . 'neotree-toggle)))

;; Keybindings to comment line region and single line
(use-package undo-tree
  :straight t
  :init
  (undo-tree-mode))

;; https://oremacs.com/swiper/#copying
;; https://github.com/abo-abo/swiper
;; Ivy: generic completion mechanism for Emacs
;; Counsel: collection of Ivy-enhanced versions of common Emacs commands
;; Swiper: Ivy-enhanced alternative to Isearch
(use-package counsel
  :straight t
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; Puts angry red squiggles on the screen when I do something stupid.
;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;; COMplete ANYthing
;; Could give wrong completions (orgmode)
;; http://company-mode.github.io/
(use-package company
  :straight t
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
  :straight t
  :hook (company-mode . company-box-mode))

;; lsp -> language server protocol
;; https://github.com/emacs-lsp/helm-lsp
(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

;; https://github.com/company-mode/company-quickhelp
(use-package company-quickhelp
   :straight t
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
  :straight t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Minor mode for Emacs that deals with parens pairs and tries to be smart about
;; it.
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :straight t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t) ;; These options can be t or nil.
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t)
  :custom-face
  (sp-show-pair-match-face ((t (:foreground "Purple" :background "Green")))) ;; Could also have :background "Grey" for example.
  )

;; END OF GENERAL
