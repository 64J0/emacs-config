;;; lang-markdown.el --- Markdown configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Markdown (MD) related packages and configuration.
;;
;; Table of packages:
;;
;; - markdown-mode
;; - ox-gfm
;; - markdown-toc

;;; Code:

;; Markdown mode with GitHub flavor.
;; `https://jblevins.org/projects/markdown-mode/'
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; GitHub Flavored Markdown
;; This package changes the default MD exporter for org-mode to use the
;; GitHub syntax (GFM).
;; https://github.com/larstvei/ox-gfm
(use-package ox-gfm
  :straight t)

;; Markdown TOC
;; A simple mode to create TOC in a well-formed markdown file.
;; https://github.com/ardumont/markdown-toc
(use-package markdown-toc
  :straight t)

;;; lang-markdown.el ends here
