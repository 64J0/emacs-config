;; ====================================================
;; ORG MODE
;;
;; A GNU Emacs major mode for keeping notes, authoring documents, computational
;; notebooks, literate programming, maintaining to-do lists, planning projects,
;; and more.
;;
;; https://orgmode.org/
;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
;; https://github.com/ebellani/Emacs.d/blob/master/init.el

(defun concat-deps-path (filename)
  "This function helps to avoid repeating the full path for the
   `deps' folder (dependencies)."
  (if (string-empty-p filename)
      (error "[-] File name is empty!")
    (concat "~/org/deps/" filename)))

;; FlySpell for spell checking
;; https://www.emacswiki.org/emacs/FlySpell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(setq flyspell-issue-message-flag nil) ;; performance improvement

(use-package org
  :straight t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :preface
  (setq org-export-backends '(moderncv md beamer ascii html latex odt org))
  :mode
  ("\\.org\\'" . org-mode)
  :init
  (setq gajo-org-srs-path "~/org/srs/deck-refile.org"
        gajo-org-notes-path "~/org/notes.org"
        gajo-org-refile-path "~/org/refile.org"
        gajo-org-agenda-path "~/org/agenda.org"
        gajo-org-todo-path "~/org/todo.org"
        gajo-org-work-path "~/org/work.org"
        gajo-org-meetings-path "~/org/meetings.org")
  :custom
  (org-support-shift-select 'always)
  :config
  ;; Required for PlantUML diagrams
  ;; From: https://plantuml.com/download
  (setq org-plantuml-jar-path
        (expand-file-name (concat-deps-path "plantuml-1.2021.16.jar")))
  (setq org-ditaa-jar-path
        (expand-file-name (concat-deps-path "ditaa0_9.jar")))
  (setq org-src-fontify-natively t)
  (setq org-directory "~/org")
  (setq org-log-done t)
  (setq org-export-backends
        '(md gfm beamer ascii taskjuggler html latex odt org))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-babel-inline-result-wrap "=%s=")
  (setq org-use-sub-superscripts '{})
  (setq org-export-with-sub-superscripts '{})
  (setq org-duration-format '((special . h:mm)))
  (setq org-goto-interface 'outline-path-completion)
  (setq org-agenda-include-diary nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-sticky nil)
  (setq org-agenda-span 21)
  (setq org-latex-pdf-process (list "latexmk -silent -f -pdf %f"))
  (setq org-latex-create-formula-image-program 'dvipng) ;; apt install dvipng
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-highlight-latex-and-related '(native))
  (setq org-cite-export-processors '((latex biblatex)
                                     (moderncv basic)
                                     (t basic)))
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)"))))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "orange" :weight bold)
	        ("NEXT" :foreground "blue" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
	        ("CANCELLED" :foreground "red" :weight bold))))
  (setq org-refile-targets
        `((nil                     :maxlevel . 9)
          (org-agenda-files        :maxlevel . 2)
          (,gajo-org-srs-path      :maxlevel . 2)
          (,gajo-org-agenda-path   :maxlevel . 2)))
  (setq org-agenda-files (list
                          gajo-org-agenda-path
                          gajo-org-work-path
                          gajo-org-todo-path))
  (setq org-capture-templates
        `(("t" "To-Do" entry (file gajo-org-todo-path)
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %t\n"
                    ":PROPERTIES:\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    " - State \"TODO\"       from \"\"  %U  \\\\\n"
                    "  %^{Initial log} %?\n"
                    ":END:")
           :jump-to-captured t)
          ("w" "Work reminder" entry (file+headline gajo-org-work-path "DevSecOps")
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %t\n"
                    ":PROPERTIES:\n"
                    ":work_reminder: t\n"
                    ":END:\n"
                    ":LOGBOOK:\n"
                    "- Initial note taken on %U \\\n"
                    "%^{Initial note}\n"
                    ":END:\n"))
          ("m" "Meeting log" entry (file gajo-org-meetings-path)
           ,(concat "* %^{Title}\n"
                    "** Context\n"
                    "%^{Context}\n"
                    "** Goal\n"
                    "%^{Goal}\n"
                    "** Agenda\n"
                    "%^{Agenda}\n"
                    "** Ata\n"
                    "%^{Minutes})\n"))
          ("d" "Drill card with answer" entry (file ,gajo-org-srs-path)
           ,(concat "* Item           :drill:\n"
                    "%^{Question}\n"
                    "** Answer\n"
                    "%^{Answer}\n"))
          ("z" "Drill" entry (file ,gajo-org-srs-path)
           ,(concat "* Item           :drill:\n"
                    "%?\n"))
          ("x" "Drill cloze 2" entry (file ,gajo-org-srs-path)
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
     (plantuml . t)
     (ditaa    . t)
     (C        . t)
     (org      . t)
     (fsharp   . t))))

;; Second brain
;; https://www.youtube.com/watch?v=AyhPmypHDEw
;; templates from:
;; https://systemcrafters.net/build-a-second-brain-in-emacs/capturing-notes-efficiently/
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
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

;; Spaced repetition algorithm to conduct interactive "drill sessions",
;; using org files as sources of facts to be memorised.
;; https://orgmode.org/worg/org-contrib/org-drill.html
(use-package org-drill
  :straight t
  :commands (org-drill)
  :init
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-scope 'directory) ;; file
  (setq org-drill-maximum-items-per-session nil)
  (setq org-drill-maximum-duration 30))

;; Prettify headings and plain lists in Org mode.
;; https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :straight t
  :init
  (add-hook
   'org-mode-hook (lambda () (org-superstar-mode 1))))

;; This repository contains add-ons to Org.
;; https://git.sr.ht/~bzg/org-contrib
(use-package org-contrib
  :straight t)

;; Supercharge your org daily/weekly/agenda
;; https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :straight t)

;; Code evaluation in org-mode
(use-package ob-fsharp
  :straight (ob-fsharp
             :type git
             :host github
             :repo "juergenhoetzel/ob-fsharp"
             :branch master))

;; ======================================================
;; Latex + Beamer config
;; Beamer is a LaTeX package for writing presentations.
;; https://orgmode.org/worg/exporters/beamer/tutorial.html
;; sudo apt-get install -f texlive-latex-extra
(use-package ox-beamer)

;; https://www.aidanscannell.com/post/org-mode-resume/
(use-package ox-latex
  :init
  ;; code here will run immediately
  :config
  ;; code here will run after the package is loaded
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

;; GitHub Flavored Markdown
;; This package changes the default MD exporter to use the GitHub syntax.
;; https://github.com/larstvei/ox-gfm
(use-package ox-gfm
  :straight t)
