;;; markdown.el --- My markdown configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Markdown (MD) related packages and configuration.

;;; Code:

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

;;; markdown.el ends here
