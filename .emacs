
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/work.org")))
 '(package-archives
   (quote
    (("" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages (quote (fsharp-mode magit helm org-drill))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ORG CONFIG
;;
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)
(setq org-export-backends '(md gfm beamer ascii taskjuggler html latex odt org))

;; MELPA CONFIG
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; HELM CONFIG
;;
(global-set-key (kbd "M-x") 'helm-M-x)
;; better buffer navigation w/ help
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;;; F# CONFIG
;;;
(unless (package-installed-p 'fsharp-mode)
  (package-install 'fsharp-mode))
(require 'fsharp-mode)
;; highlight indentation
(add-hook 'fsharp-mode-hook 'highlight-indentation-mode)
;; change tab default size
(setq-default fsharp-indent-offset 4)

;; MISC
;;
;; break long lines automatically
(setq-default truncate-lines nil)
