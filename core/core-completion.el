;;; core-completion.el --- Completion and editing helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Minibuffer completion, in-buffer completion, and small editing helpers
;; that aren't tied to a specific language.
;;
;; Table of packages:
;;
;; - diminish
;; - super-save
;; - rainbow-delimiters
;; - smartparens
;; - counsel (ivy + swiper)
;; - ivy-rich
;; - ivy-prescient
;; - corfu
;; - corfu-prescient
;; - which-key
;; - multiple-cursors
;; - highlight-indent-guides
;; - dired-du

;;; Code:

(require 'use-package)

;; When we diminish a mode, we are saying we want it to continue doing its work
;; for us, but we no longer want to be reminded of it. It becomes a night
;; worker, like a janitor; it becomes an invisible man; it remains a component,
;; perhaps an important one, sometimes an indispensable one, of the mechanism
;; that maintains the day-people's world, but its place in their thoughts is
;; diminished, usually to nothing. As we grow old we diminish more and more such
;; thoughts, such people, usually to nothing. -- Will Mengarini
;;
;; Repository: `https://github.com/emacsmirror/diminish'
;;
;; Also: `https://www.gnu.org/software/emacs/manual/html_node/use-package/Diminish.html'
(use-package diminish
  :straight t)

;; super-save auto-saves your buffers, when certain events happen - e.g. you
;; switch between buffers, an Emacs frame loses focus, etc.
;;
;; Repository: `https://github.com/bbatsov/super-save'
(use-package super-save
  :straight t
  :diminish super-save-mode
  :custom
  (super-save-mode +1)
  (super-save-auto-save-when-idle t)
  (auto-save-default nil))

;; rainbow-delimiters is a "rainbow-parentheses"-like mode which highlight
;; delimiters such as parentheses, brackets or braces according to their depth.
;;
;; Repository: `https://github.com/Fanael/rainbow-delimiters'
(use-package rainbow-delimiters
  :straight t
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

;; Minor mode for Emacs that deals with parens pairs and tries to be smart about
;; it.
;;
;; Repository: `https://github.com/Fuco1/smartparens'
(use-package smartparens
  :straight t
  :diminish smartparens-mode
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
  :diminish ivy-mode
  :bind (("C-x C-f" . counsel-find-file)
         ("C-s"     . swiper-isearch))
  :custom (ivy-mode t))

;; Richer annotations (docstrings, key bindings, file sizes, etc.) next to
;; Ivy candidates -- closer to VS Code's command palette descriptions.
;;
;; Repository: `https://github.com/Yevgnen/ivy-rich'
(use-package ivy-rich
  :straight t
  :after counsel
  :init (ivy-rich-mode 1))

;; Sort Ivy candidates by frecency (frequency + recency), like VS Code's
;; "most recently used" ordering in the command palette / quick open.
;;
;; Repository: `https://github.com/raxod502/prescient.el'
(use-package ivy-prescient
  :straight t
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;; Corfu enhances in-buffer completion with a small completion popup.
;;
;; Repository: `https://github.com/minad/corfu'
(require 'compat)
(unless (fboundp 'set-local)
  (defun set-local (variable value)
    "Runtime function fallback for byte-code expecting the Emacs 31 macro."
    (set (make-local-variable variable) value)))
(use-package corfu
  :straight t
  :hook
  (prog-mode . (lambda () (setq-local corfu-auto t)))
  :init
  (global-corfu-mode))

;; Frecency-based sorting for Corfu candidates, same idea as `ivy-prescient'
;; but for in-buffer completion.
;;
;; Repository: `https://github.com/raxod502/prescient.el'
(use-package corfu-prescient
  :straight t
  :after corfu
  :config
  (corfu-prescient-mode 1))

;; Shows the available key bindings for the prefix you just pressed in a
;; popup -- the closest thing Emacs has to VS Code's command-palette hints.
;;
;; Repository: `https://github.com/justbur/emacs-which-key'
(use-package which-key
  :straight t
  :diminish which-key-mode
  :init (which-key-mode))

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
;; `character' instead of the default `bitmap': bitmap draws into the
;; fringe and can end up invisible depending on fringe width/theme, whereas
;; a colored character column reliably shows up regardless of frame setup.
;; The face color is set explicitly so it doesn't depend on a theme's
;; automatic guess either.
;;
;; Repository: `https://github.com/DarthFennec/highlight-indent-guides'
(use-package highlight-indent-guides
  :straight t
  :diminish highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  :custom-face
  (highlight-indent-guides-character-face ((t (:foreground "gray70"))))
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

;;; core-completion.el ends here
