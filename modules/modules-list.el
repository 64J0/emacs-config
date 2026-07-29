;;; modules-list.el --- Enabled modules -*- lexical-binding: t; -*-

;;; Commentary:

;; Prelude-style module toggle (see `prelude-modules.el' in
;; `https://github.com/bbatsov/prelude'): comment out an entry below to
;; disable that language/topic without deleting its file. Order matters
;; only where noted:
;;
;; - `lang-org.el' loads before `lang-org-ext.el' (the latter advises
;;   functions the former defines/requires).
;; - `lang-org.el' loads before `lang-markdown.el': `ox-gfm' (an Org export
;;   backend, pulled in by the markdown module) would otherwise load
;;   Emacs's built-in Org before `lang-org.el' gets a chance to, triggering
;;   an "Org version mismatch" warning. `init.el' also pins Org via
;;   `straight-use-package' as a first line of defense, but keep this order
;;   too.

;;; Code:

(defconst gajo--module-files
  '("modules/prog-fsharp.el"
    "modules/prog-csharp.el"
    "modules/prog-c.el"
    "modules/prog-python.el"
    "modules/prog-go.el"
    "modules/prog-erlang.el"
    "modules/prog-asm.el"
    "modules/prog-data.el"
    "modules/lang-org.el"     ;; must load before lang-org-ext.el and lang-markdown.el
    "modules/lang-org-ext.el"
    "modules/lang-markdown.el")
  "Enabled per-language/topic module files, in load order.")

;;; modules-list.el ends here
