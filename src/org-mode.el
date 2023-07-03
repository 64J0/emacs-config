;;; org-mode.el --- My org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; ORG MODE
;;
;; A GNU Emacs major mode for keeping notes, authoring documents, computational
;; notebooks, literate programming, maintaining to-do lists, planning projects,
;; and more.
;;
;; https://orgmode.org/
;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
;; https://github.com/ebellani/Emacs.d/blob/master/init.el

;;; Code:

(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :preface
  (setq org-export-backends
        '(moderncv md gfm beamer ascii html latex odt org))
  :init
  (setq gajo--org-srs-path "~/org/srs/deck-refile.org"
        gajo--org-notes-path "~/org/notes.org"
        gajo--org-refile-path "~/org/refile.org"
        gajo--org-agenda-path "~/org/agenda.org"
        gajo--org-todo-path "~/org/todo.org"
        gajo--org-work-path "~/org/work.org"
        gajo--org-meetings-path "~/org/meetings.org")
  :custom
  (org-support-shift-select 'always)
  (org-directory "~/org")
  (org-src-fontify-natively t)
  (org-refile-use-outline-path 'file)
  (org-babel-inline-result-wrap "=%s=")
  (org-use-sub-superscripts '{})
  (org-export-with-sub-superscripts '{})
  (org-latex-create-formula-image-program 'dvipng) ;; apt install dvipng
  (org-highlight-latex-and-related '(native))
  (org-cite-export-processors '((latex biblatex)
                                (moderncv basic)
                                (t basic)))
  :config
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))
  (setq org-todo-keywords
        (quote ((sequence "TODO(t!)"
                          "NEXT(n@)"
                          "HOLD(h@)" "|"
                          "DONE(d!)"
                          "CANCELLED(c@)"))))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "orange" :weight bold)
	        ("NEXT" :foreground "blue" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
	        ("CANCELLED" :foreground "red" :weight bold))))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-refile-targets
        `((nil                     :maxlevel . 9)
          (org-agenda-files        :maxlevel . 2)
          (,gajo--org-srs-path      :maxlevel . 2)
          (,gajo--org-agenda-path   :maxlevel . 2)))
  (setq org-agenda-files (list gajo--org-agenda-path
                               gajo--org-work-path
                               gajo--org-todo-path))
  (setq org-capture-templates
        `(("t" "To-Do" entry (file gajo--org-todo-path)
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %t\n"
                    ":PROPERTIES:\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    " - State \"TODO\"       from \"\"  %U  \\\\\n"
                    "  %^{Initial log} %?\n"
                    ":END:")
           :jump-to-captured t)
          ("w" "Work reminder" entry (file+headline gajo--org-work-path "DevSecOps")
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %t\n"
                    ":PROPERTIES:\n"
                    ":work_reminder: t\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    "- Initial note taken on %U \\\n"
                    "%^{Initial note}\n"
                    ":END:\n"))
          ("m" "Meeting log" entry (file gajo--org-meetings-path)
           ,(concat "* %^{Title}\n"
                    "** Context\n"
                    "%^{Context}\n"
                    "** Goal\n"
                    "%^{Goal}\n"
                    "** Agenda\n"
                    "%^{Agenda}\n"
                    "** Ata\n"
                    "%^{Minutes})\n"))
          ("d" "Drill card with answer" entry (file ,gajo--org-srs-path)
           ,(concat "* Item           :drill:\n"
                    "%^{Question}\n"
                    "** Answer\n"
                    "%^{Answer}\n"))
          ("z" "Drill" entry (file ,gajo--org-srs-path)
           ,(concat "* Item           :drill:\n"
                    "%?\n"))
          ("x" "Drill cloze 2" entry (file ,gajo--org-srs-path)
           ,(concat "* Item           :drill:\n"
                    ":PROPERTIES:\n"
                    ":drill_card_type: hide2cloze\n"
                    ":END:\n"
                    "%?\n"))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql      . t)
     (latex    . t)
     (shell    . t)
     (python   . t)
     (js       . t)
     (C        . t)
     (org      . t))))

;; FlySpell for spell checking
;; https://www.emacswiki.org/emacs/FlySpell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(setq flyspell-issue-message-flag nil) ;; performance improvement

;; Org-roam is a plain-text knowledge management system.  It brings some of
;; Roam's more powerful features into the Org-mode ecosystem.
;;
;; Repository: `https://github.com/org-roam/org-roam'
;; Video: `https://www.youtube.com/watch?v=AyhPmypHDEw'
;; Article: `https://systemcrafters.net/build-a-second-brain-in-emacs/capturing-notes-efficiently/'
(use-package org-roam
  :straight t
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/RoamNotes")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"   . completion-at-point))
  :config
  (org-roam-db-autosync-mode))

;; Spaced repetition algorithm to conduct interactive "drill sessions", using
;; org files as sources of facts to be memorised.
;;
;; Repository: `https://gitlab.com/phillord/org-drill/'
(use-package org-drill
  :straight t
  :commands (org-drill)
  :config
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-scope 'directory) ;; file
  (setq org-drill-maximum-items-per-session nil)
  (setq org-drill-maximum-duration 30))

;; Prettify headings and plain lists in Org mode.
;;
;; Repository: `https://github.com/integral-dw/org-superstar-mode'
(use-package org-superstar
  :straight t
  :init
  (add-hook
   'org-mode-hook (lambda () (org-superstar-mode 1))))

;; Unmaintained add-ons for Org-mode.
;;
;; Repository: `https://github.com/emacsmirror/org-contrib'
(use-package org-contrib
  :straight t)

;; Supercharge your org daily/weekly agenda by grouping items.
;;
;; Repository: `https://github.com/alphapapa/org-super-agenda'
(use-package org-super-agenda
  :straight t)

;; F# code evaluation in org-mode
;;
;; Repository: `https://github.com/juergenhoetzel/ob-fsharp'
(use-package ob-fsharp
  :straight t
  :config
  (add-to-list 'org-babel-load-languages '(fsharp . t)))

;; Docs: `https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html'
(use-package ob-clojure
  :config
  (setq org-babel-clojure-backend 'cider)
  (add-to-list 'org-babel-load-languages '(clojure . t)))

(use-package ob-rust
  :straight t
  :config
  (add-to-list 'org-babel-load-languages '(rust . t)))

;; ======================================================
;; Latex + Beamer config
;; Beamer is a LaTeX package for writing presentations.
;; https://orgmode.org/worg/exporters/beamer/tutorial.html
;; sudo apt-get install -f texlive-latex-extra
(use-package ox-beamer)

;; https://www.aidanscannell.com/post/org-mode-resume/
(use-package ox-latex
  :config
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author...} to latex export
  ;; delete unwanted file extensions after latexMK
  (setq org-latex-logfiles-extensions
        (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))
  
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil)))

(use-package ox-extra
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package oc-biblatex)

;;; org-mode.el ends here
