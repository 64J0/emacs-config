;;; prog-data.el --- JSON/YAML/Dockerfile support -*- lexical-binding: t; -*-

;;; Commentary:

;; Syntax highlighting for common DevOps/data file formats. No LSP servers
;; here, just major modes.
;;
;; Table of packages:
;;
;; - json-mode
;; - dockerfile-mode
;; - yaml-mode

;;; Code:

;; Used for json files.
;; `https://github.com/joshwnj/json-mode'
(use-package json-mode
  :straight t
  :mode ("\\.json\\'" . json-mode))

;; Pretty syntax highlight for editing Dockerfiles.
;; `https://github.com/spotify/dockerfile-mode'
(use-package dockerfile-mode
  :straight t
  :mode ("\\Dockerfile\\'" "\\.dockerfile\\'"))

;; YAML mode to handle YAML manifests.
;; `https://www.emacswiki.org/emacs/YamlMode'
(use-package yaml-mode
  :straight t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;;; prog-data.el ends here
