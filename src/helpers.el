;;; helpers.el --- Some helper packages -*- lexical-binding: t; -*-

;;; Commentary:

;; At this file I put the helper packages which are not related to the other
;; categories.
;;
;; Table of packages:
;;
;; - which-key
;; - super-save
;; - helm
;; - rainbow-delimiters
;; - smartparens
;; - counsel
;; - company
;; - company-box
;; - multiple-cursors
;; - highlight-indent-guides
;; - dired-du

;;; Code:

(require 'use-package)

;; It's a minor mode for Emacs that displays the key bindings following your
;; currently entered incomplete command (a prefix) in a popup.
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
  (company-idle-delay 0.0) ; default is 0.2
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

;; Display the indentation level.
;;
;; Repository: `https://github.com/DarthFennec/highlight-indent-guides'
(use-package highlight-indent-guides
  :straight t
  :custom
  (highlight-indent-guides-method 'bitmap)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; Display the recursive size of directories in Dired
;;
;; `https://elpa.gnu.org/packages/dired-du.html'
(use-package dired-du
  :straight t
  ;; was too slow
  ;; :hook
  ;; (dired-mode . dired-du-mode)
  )

;;; helpers.el ends here
