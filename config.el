(setq user-full-name "Vincent Montero"
      user-mail-address "vincent_montero@icloud.com")

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(require 'cc-mode)

(use-package f)
(use-package diminish)
(use-package lispy)
(use-package aggressive-indent)
(use-package ibuffer-projectile)
(use-package ivy-yasnippet
  :bind ("H-," . ivy-yasnippet))
(use-package ess-smart-equals)

(setq scimax-dir "~/scimax/")
(setq scimax-user-dir "~/scimax/")

(add-to-list 'load-path (expand-file-name "~/scimax"))
(load "scimax-org")

(eval-after-load 'bibtex
  '(progn
     (require 'bibtex-hotkeys)))

(require 'jupyter)
(require 'ob-jupyter)
(require 'scimax-jupyter)
;;(global-unset-key (kbd "<f12>"))
(global-set-key (kbd "s-<") 'scimax/body)
(jupyter-org-define-key (kbd "s-<") #'scimax-jupyter-org-hydra/body)

(use-package words
  :bind ("H-w" . words-hydra/body))
(require 'scimax-ob)
;; (require 'scimax-autoformat-abbrev)
(require 'scimax-utils)
;; (require 'scimax-contacts)
(require 'scimax-hydra)
(require 'scimax-statistics)
(require 'scimax-journal)

(easy-menu-add)
(eval-after-load 'easy-menu
  '(progn
     (require 'scimax-notebook)))

(require 'scimax-ivy) ; M-TAB for multiple selection
(require 'scimax-yas)
(require 'scimax-elfeed)
(require 'scimax-spellcheck)
(require 'scimax-apps)

(setq doom-theme 'doom-dracula)

(setq display-time-day-and-date t)
(display-time)
(display-time-mode 1)
;;(add-hook 'after-init-hook (lambda () (org-agenda nil "o")))

(display-battery-mode 1)

(setq scroll-conservatively 100)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq display-line-numbers-type t)
(global-display-line-numbers-mode)
(setq doom-modeline-enable-word-count t)

;;Highlight current line
(global-hl-line-mode)

;;Scroll and Tool bar modes
(recentf-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(after! org
  (setq org-src-block-faces nil)
  )

(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(setq org-pretty-entities t)

(setq org-hide-emphasis-markers t)


(use-package org-appear
  :hook (org-mode . org-appear-mode))

(with-eval-after-load 'org-superstar
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?-)))
  (setq org-superstar-special-todo-items t))

(beacon-mode 1)

(setq org-startup-with-inline-images nil)

;; Meta key on apple keyboard
(setq ns-alternate-modifier 'meta)
(setq ns-right-alternate-modifier 'none)

;; set keys for Apple keyboard, for emacs in OS X
(setq mac-control-modifier 'control) ; make Control key do Control
(setq mac-option-modifier 'meta) ; make cmd left key do Meta
(setq mac-left-command-modifier 'super) ; make left opt key do Super
(setq mac-right-command-modifier 'hyper)  ; make cmd right key do Hyper

(global-set-key (kbd "M-q") 'toggle-truncate-lines)

(add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.8.14/share/emacs/site-lisp/mu/mu4e")
;; (require 'mu4e)
(require 'smtpmail)

(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.6)

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org")))

(map! :leader
      :desc "Elfeed"
       "e e" #'elfeed
       "e u" #'elfeed-update)

(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev
  (kbd "E") 'email-elfeed-entry
  (kbd "C") (lambda () (interactive) (org-capture))
  (kbd "D") 'doi-utils-add-entry-from-elfeed-entry
  ;; help me alternate fingers in marking entries as read
  (kbd "F") 'elfeed-search-untag-all-unread
  (kbd "O") 'elfeed-search-show-entry)

(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq-default ispell-dictionary "english")


(let ((langs '("british" "french" "spanish")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))
(global-set-key (kbd "H-m") 'cycle-ispell-languages)

(global-set-key (kbd "H--") 'org-subscript-region-or-point)
(global-set-key (kbd "H-=") 'org-superscript-region-or-point)
(global-set-key (kbd "H-i") 'org-italics-region-or-point)
(global-set-key (kbd "H-b") 'org-bold-region-or-point)
(global-set-key (kbd "H-v") 'org-verbatim-region-or-point)
(global-set-key (kbd "H-c") 'org-code-region-or-point)
(global-set-key (kbd "H-u") 'org-underline-region-or-point)
(global-set-key (kbd "H-+") 'org-strikethrough-region-or-point)
(global-set-key (kbd "H-4") 'org-latex-math-region-or-point)
(global-set-key (kbd "H-e") 'ivy-insert-org-entity)
(global-set-key (kbd "H-\"") 'org-double-quote-region-or-point)
(global-set-key (kbd "H-'") 'org-single-quote-region-or-point)

(defun my/org-mode-IC50-autoformat ()
  "Autoformat IC50 as IC_{50} in Org-mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "IC50" nil t)
      (replace-match "IC_{50}"))))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(setq org-roam-graph-executable "/opt/homebrew/Cellar/graphviz/8.0.5/bin/dot")
(setq org-roam-graph-viewer "/System/Volumes/Preboot/Cryptexes/App/System/Applications/Safari.app/Contents/MacOS/Safari")

(use-package org-mac-link)

(eval-after-load 'org '(require 'org-pdfview))

(setq! bibtex-completion-pdf-open-function  (lambda (fpath)
                                           (call-process "open" nil 0 nil fpath))
     )

(use-package! org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(add-hook 'pdf-tools-enabled-hook 'pdf-view-dark-minor-mode)

(defun efs/presentation-setup ()
  (setq text-scale-mode-amount 3)
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images
  (text-scale-mode 1))

(defun efs/presentation-end ()
  (text-scale-mode 0))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " // ")
  (org-image-actual-width nil))

;; (setq face-remapping-alist '((default (:height 2.4) default)
;;                              (italic (sheight 2.4) italic)))

(setq org-latex-title-command "")

(setq org-latex-prefer-user-labels t)

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "bibtex $(basename %b)"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")
      )

(setq org-latex-default-packages-alist
      '(
        ("" "graphicx" nil)       ;; For including images and graphics.
        ("" "longtable" nil)      ;; For creating tables that span multiple pages.
        ("T1" "fontenc" nil)      ;; For specifying font encoding.
        ("AUTO" "inputenc" nil)   ;; For specifying the input encoding, typically UTF-8.
        ("" "lmodern" nil)      ;; This is for handling accented characters
        ("" "textcomp" nil)     ;; Provides additional symbols and text-related commands.
        ("" "amsmath" nil)      ;; Fundamental for mathematical formatting and equations.
        ("linktocpage, pdfstartview=FitH, colorlinks, linkcolor=blue, anchorcolor=blue, citecolor=blue, filecolor=blue, menucolor=blue, urlcolor=blue" "hyperref" nil) ;; Adds support for hyperlinks within the document when exporting to PDF.
        ("top=1in, bottom=1.in, left=1in, right=1in" "geometry" nil) ;; For setting page dimensions and margins.
        ("" "setspace" nil)     ;; Allows adjusting line spacing. /!\ Conflict with biblatex in beamer ?????
        ("" "xcolor" nil)       ;; For color support in the document.
        ("" "indentfirst" ni)   ;; Indents the first paragraph of each section.
        ("numbers,super,sort&compress" "natbib" nil) ;; For bibliographic citations and references.
        ("" "enumitem" nil)     ;; Control layout of itemize, enumerate, description, Customizes the formatting of lists (enumerate, itemize).
        ("" "tabularx" nil)     ;; Enhanced support for tables with variable column widths.
        ("" "calc" nil)         ;; Adds mathematical calculations to LaTeX commands.
        ("" "ifthen" nil)       ;; Provides conditional commands.
        ("" "listings" nil)     ;; For formatting code listings.
        ("" "float" nil)        ;; Improves the formatting of floating elements like figures and tables.
        ("" "fancyvrb" nil)     ;; Enhances the formatting of verbatim text.
        ("" "amssymb" nil)      ;; Provides additional mathematical symbols.
        ("" "amsthm" nil)       ;; Adds support for theorems and theorem-like environments.
        ("" "parskip" nil)      ;; Adjusts paragraph spacing and indentation.
        ("" "footmisc" nil)     ;; Customizes footnote formatting.
        ("" "textgreek" nil)    ;; Allows input of Greek characters in text mode.
        ("" "babel" nil)        ;; For multilingual support and language-specific formatting.
        ("" "csquotes" nil)     ;; Enhances quotation marks and citation styles.
        ("" "url" nil)          ;; Allows formatting of URLs.
        )
      )

(add-to-list 'org-latex-packages-alist '("" "marginnote" nil))  ;; For left column
(add-to-list 'org-latex-packages-alist '("" "marginfix" nil))   ;; For command \clearmargin for manually moving the left column to the next page
(add-to-list 'org-latex-packages-alist '("" "fancyhdr" nil))    ;; Extensive control of page headers and footers in LATEX2ε
(add-to-list 'org-latex-packages-alist '("" "lastpage" nil))    ;; Reference last page for Page N of M type footers
(add-to-list 'org-latex-packages-alist '("" "etoolbox" nil))    ;; for \AtBeginDocument etc.
(add-to-list 'org-latex-packages-alist '("" "tabto" nil))       ;; To use tab for alignment on first page
(add-to-list 'org-latex-packages-alist '("" "totcount" nil))    ;; To enable extracting the value of the counter "page"
(add-to-list 'org-latex-packages-alist '("" "ragged2e" nil))    ;; For command \justifying
(add-to-list 'org-latex-packages-alist '("" "pbox" nil))        ;; For biography environment
(add-to-list 'org-latex-packages-alist '("" "enotez" nil))      ;; For endnotes
(add-to-list 'org-latex-packages-alist '("" "rotating" nil))    ;; Rotation tools, including rotated full-page floats

(add-to-list 'org-latex-packages-alist '("right" "lineno" t))           ;; Line numbers on paragraphs
(add-to-list 'org-latex-packages-alist '("" "soul" nil))                ;; To highlight text
(add-to-list 'org-latex-packages-alist '("normalem" "ulem" nil))        ;; Package for underlining
(add-to-list 'org-latex-packages-alist '("" "microtype" nil))           ;; For command \textls[]{}
(add-to-list 'org-latex-packages-alist '("strings" "underscore" nil))
(add-to-list 'org-latex-packages-alist '("" "marvosym" t))              ;; Martin Vogel's Symbols (marvosym) font, contains the Euro currency symbol
(add-to-list 'org-latex-packages-alist '("" "wasysym" t))               ;; support for the wasy fonts (Waldi Symbol) by Roland Waldi provides many glyphs like male and female symbols and astronomical symbols

(add-to-list 'org-latex-packages-alist '("" "natmove" nil))     ;; The package has only one purpose: to move superscripted citations beyond punctuation

(add-to-list 'org-latex-packages-alist '("" "url" nil))

(add-to-list 'org-latex-packages-alist '("" "pdfpages" nil))           ; Include PDF documents in LATEX
(add-to-list 'org-latex-packages-alist '("" "attachfile" nil))  ;; Attach arbitrary files to a PDF document

(add-to-list 'org-latex-packages-alist '("" "adjustbox" nil))     ;; Graphics package-alike macros for “general” boxes
(add-to-list 'org-latex-packages-alist '("skip=0.5 \\baselineskip" "caption" nil)) ; Customising captions in floating environments
(add-to-list 'org-latex-packages-alist '("" "float" nil))       ;; Improved interface for floating objects
(add-to-list 'org-latex-packages-alist '("" "wrapfig" nil))     ;; makes it possible to wrap text around figures

(add-to-list 'org-latex-packages-alist '("" "epstopdf" nil)) ; Convert EPS to PDF using Ghostscript
(add-to-list 'org-latex-packages-alist '("" "tikz" nil))            ; For \foreach used for Orcid icon
(add-to-list 'org-latex-packages-alist '("" "changepage" nil)) ; To adjust the width of the column for the title part and figures/tables (adjustwidth environment)
(add-to-list 'org-latex-packages-alist '("" "graphbox" nil)) ; To align graphics inside tables

(add-to-list 'org-latex-packages-alist '("" "longtable" nil))             ; Tabulars with adjustable-width columns
(add-to-list 'org-latex-packages-alist '("" "booktabs" nil))  ; for \toprule etc. in tables
(add-to-list 'org-latex-packages-alist '("" "multirow" nil))        ; Create tabular cells spanning multiple rows
(add-to-list 'org-latex-packages-alist '("" "array" nil))      ; For table array
(add-to-list 'org-latex-packages-alist '("" "xcolor, colortbl" nil)) ; To provide color for soul (for english editing), for adding cell color of table
(add-to-list 'org-latex-packages-alist '("" "longtable" nil))   ;; Allow tables to flow over page boundaries
(setq org-latex-tables-booktabs t)

(add-to-list 'org-latex-packages-alist '("" "glossaries" nil))
(add-to-list 'org-latex-packages-alist '("" "makeidx" nil))

(add-to-list 'org-latex-packages-alist '("version=4" "mhchem" t)) ; provides commands for typesetting chemical molecular formulae and equations.
(add-to-list 'org-latex-packages-alist '("" "chemmacros" t)) ; A collection of macros to support typesetting chemistry documents, nomenclature commands, oxidation numbers, thermodynamic data, newman projections, etc.
(add-to-list 'org-latex-packages-alist '("" "chemnum" t))   ; A method for numbering chemical compounds
(add-to-list 'org-latex-packages-alist '("" "bpchem" t)) ;numbering molecules with \CNref

(setq org-latex-listings 'minted)
(setq org-latex-custom-lang-environments
            '((emacs-lisp "common-lispcode")))

(add-to-list 'org-latex-packages-alist '("cache=false" "minted" nil))   ;; Highlighted source code for LATEX

(with-eval-after-load 'ox-latex

     (add-to-list 'org-latex-classes
                  '("copernicus_discussions"
                    "\\documentclass{copernicus_discussions}
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     [EXTRA]"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" "\\newpage" "\\subsection*{%s}" "\\newpage")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

     (add-to-list 'org-latex-classes
                  '("mdpi"
                    "\\documentclass{Definitions/mdpi}
                     [PACKAGES]
                     [EXTRA]"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

     (add-to-list 'org-latex-classes
                  '("book"
                    "\\documentclass{book}"
                    ("\\part{%s}" . "\\part*{%s}")
                    ("\\chapter{%s}" . "\\chapter*{%s}")
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

     (add-to-list 'org-latex-classes
                  '("amu-these"
                    "\\documentclass{amu_these}
                     [NO-DEFAULT-PACKAGES]
                     [NO-PACKAGES]
                     [EXTRA]"
                    ("\\part{%s}" . "\\part*{%s}")
                    ("\\chapter{%s}" . "\\chapter*{%s}")
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ))

     (add-to-list 'org-latex-classes
                  '("jmedchem"
                    "\\documentclass{achemso}
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     [EXTRA]"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                    ))

     (add-to-list 'org-latex-classes
                  '("elsarticle"
                    "\\documentclass{elsarticle}
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     [EXTRA]"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                    ))

     (add-to-list 'org-latex-classes
                  '("rsc"
                    "\\documentclass[twoside,twocolumn,9pt]{article}
\\usepackage{extsizes}
\\usepackage[super,sort&compress,comma]{natbib}
\\usepackage[version=3]{mhchem}
\\usepackage{bpchem}
\\usepackage{chemmacros}
\\usepackage[left=1.5cm, right=1.5cm, top=1.785cm, bottom=2.0cm]{geometry}
\\usepackage{balance}
\\usepackage{mathptmx}
\\usepackage{sectsty}
\\usepackage{graphicx}
\\usepackage{lastpage}
\\usepackage[format=plain,justification=justified,singlelinecheck=false,font={stretch=1.125,small,sf},labelfont=bf,labelsep=space]{caption}
\\usepackage{float}
\\usepackage{fancyhdr}
\\usepackage{fnpos}
\\usepackage[english]{babel}
\\addto{\captionsenglish}{%
  \\renewcommand{\refname}{Notes and references}
}
\\usepackage{array}
\\usepackage{droidsans}
\\usepackage{charter}
\\usepackage[T1]{fontenc}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage{setspace}
\\usepackage[compact]{titlesec}
\\usepackage{hyperref}
%%%Please don't disable any packages in the preamble, as this may cause the template to display incorrectly.%%%


\\usepackage{epstopdf}%This line makes .eps figures into .pdf - please comment out if not required.

\\definecolor{cream}{RGB}{222,217,201}

\\usepackage{pdfpages}
\\usepackage{booktabs}
\\usepackage{multirow}
                     [NO-DEFAULT-PACKAGES]
                     [NO-PACKAGES]
                     [EXTRA]"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                    ))


     )

(defun my-org-export-to-pdf-biber ()
  "Export the current buffer to PDF using Org mode and open the resulting PDF file."
  (interactive)
  (let ((org-export-before-parsing-hook '(org-ref-glossary-before-parsing
                                           org-ref-acronyms-before-parsing))
        (org-latex-pdf-process
         '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "biber %b"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")))
    (org-latex-export-to-pdf)
    (org-open-file (concat (file-name-sans-extension buffer-file-name) ".pdf"))))

(defun my-org-export-to-pdf-gloss-bibtex ()
  "Export the current buffer to PDF using Org mode and open the resulting PDF file."
  (interactive)
  (let ((org-export-before-parsing-hook '(org-ref-glossary-before-parsing
                                           org-ref-acronyms-before-parsing))
        (org-latex-pdf-process
         '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "bibtex %b"
           "makeindex %b"
           "makeglossaries %b"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "makeindex %b"
           "makeglossaries %b"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           )))
    (org-latex-export-to-pdf)
    (org-open-file (concat (file-name-sans-extension buffer-file-name) ".pdf"))))

(defun my-org-export-to-pdf-gloss-biber ()
  "Export the current buffer to PDF using Org mode and open the resulting PDF file."
  (interactive)
  (let ((org-export-before-parsing-hook '(org-ref-glossary-before-parsing
                                           org-ref-acronyms-before-parsing))
        (org-latex-pdf-process
         '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "biber %b"
           "makeindex %b"
           "makeglossaries %b"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "makeindex %b"
           "makeglossaries %b"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           )))
    (org-latex-export-to-pdf)
    (org-open-file (concat (file-name-sans-extension buffer-file-name) ".pdf"))))

(defvar bibtex-abbreviations
  '(
    ("ACAT" "ACS Catalysis" "ACS Catal.")
    ("AM" "Acta Materialia" "Acta Mater.")
    ("AMM" "Acta Metallurgica et Materialia" "Acta Metall. Mater.")
    ("AMiner" "American Mineralogist" "Am. Mineral.")
    ("AngC" "Angewandte Chemie International Edition" "Angew. Chem. Int. Edit.")
    ("APLM" "APL Materials" "APL Mat.")
    ("ACBE" "Applied Catalysis B: Environmental" "Appl. Catal. B-Environ.")
    ("APL" "Applied Physics Letters" "Appl. Phys. Lett.")
    ("ASS" "Applied Surface Science" "Appl. Surf. Sci.")
    ("CL" "Catalysis Letters" "Catal. Lett.")
    ("CT" "Catalysis Today" "Catal. Today")
    ("CPL" "Chemical Physics Letters" "Chem. Phys. Lett")
    ("CR" "Chemical Reviews" "Chem. Rev.")
    ("CSR" "Chemical Society Reviews" "Chem. Soc. Rev.")
    ("CSR" "Chemical Society Reviews" "Chem. Soc. Rev.")
    ("CM" "Chemistry of Materials" "Chem. Mater.")
    ("CSA" "Colloids and Surfaces, A: Physicochemical and Engineering Aspects" "Colloids Surf., A")
    ("CPMS" "Computational Materials Science" "Comp. Mater. Sci.")
    ("CPC" "Computer Physics Communications" "Comput. Phys. Commun.")
    ("CGD" "Crystal Growth \\& Design" "Cryst. Growth Des.")
    ("CEC" "CrystEngComm" "CrystEngComm")
    ("ECST" "ECS Transactions" "ECS Trans.")
    ("EES" "Energy \\& Environmental Science" "Energy Environ. Sci.")
    ("HPR" "High Pressure Research" "High Pressure Res.")
    ("IC" "Inorganic Chemistry" "Inorg. Chem.")
    ("IECR" "Industrial \\& Engineering Chemistry Research" "Ind. Eng. Chem. Res.")
    ("JJAP" "Japanese Journal of Applied Physics" "Jpn. J. Appl. Phys.")
    ("JMatR" "Journal of  Materials Research" "J. Mater. Res.")
    ("JALC" "Journal of Alloys and Compounds" "J. Alloy Compd.")
    ("JAC" "Journal of Applied Crystallography" "J. Appl. Crystallogr.")
    ("JAP" "Journal of Applied Physics" "J. Appl. Phys.")
    ("JC" "Journal of Catalysis" "J. Catal.")
    ("JCP" "Journal of Chemical Physics" "J. Chem. Phys.")
    ("JCG" "Journal of Crystal Growth" "J. Crys. Growth")
    ("JMC" "Journal of Materials Chemistry" "J. Mater. Chem.")
    ("JMC" "Journal of Materials Chemistry" "J. Mater. Chem.")
    ("JMSL" "Journal of Materials Science Letters" "J. Mater. Sci. Lett.")
    ("JMS" "Journal of Membrane Science" "J. Memb. Sci.")
    ("JPE" "Journal of Phase Equilibria" "J. Phase Equilib.")
    ("JPCS" "Journal of Physics and Chemistry of Solids" "J. Phys. Chem. Solids")
    ("JPCM" "Journal of Physics: Condensed Matter" "J. Phys.: Condens. Matter")
    ("JSSC" "Journal of Solid State Chemistry" "J. Solid State Chem.")
    ("JACerS" "Journal of the American Ceramic Society" "J. Am. Ceram. Soc.")
    ("JACS" "Journal of the American Chemical Society" "J. Am. Chem. Soc.")
    ("JES" "Journal of The Electrochemical Society" "J. Electrochem. Soc.")
    ("JES" "Journal of The Electrochemical Society" "J. Electrochem. Soc.")
    ("JMS" "Journal of Membrane Science" "J. Memb. Sci.")
    ("JVST" "Journal of Vacuum Science \\& Technology A" "J. Vac. Sci. Technol. A")
    ("ML" "Materials Letters" "Mater. Lett.")
    ("MSE-BS" "Materials Science and Engineering B" "Mat. Sci. Eng. B-Solid")
    ("MOLSIM" "Molecular Simulation" "Mol. Sim.")
    ("Nature" "Nature" "Nature")
    ("NM" "Nature Materials" "Nat. Mater.")
    ("PML" "Philosophical Magazine Letters" "Phil. Mag. Lett.")
    ("PMA" "Philosophical Magazine A" "Phil. Mag. A")
    ("PA" "Physica A: Statistical Mechanics and its Applications" "Physica A")
    ("PB" "Physica B-Condensed Matter" "Physica B")
    ("PCCP" "Physical Chemistry Chemical Physics" "Phys. Chem. Chem. Phys.")
    ("PSSB" "physica status solidi (b)" "Phys. Status Solidi B")
    ("PRA" "Physical Review A" "Phys. Rev. A")
    ("PRB" "Physical Review B" "Phys. Rev. B")
    ("PRL" "Physical Review Letters" "Phys. Rev. Lett.")
    ("PCM" "Physics and Chemistry of Minerals" "Phys. Chem. Miner.")
    ("PSurfSci" "Progress in Surface Science" "Prog. Surf. Sci.")
    ("Science" "Science" "Science")
    ("SABC" "Sensors and Actuators B: Chemical" "Sensor. Actuat. B-Chem.")
    ("SS" "Surface Science" "Surf. Sci.")
    ("EPJB" "The European Physical Journal B" "Eur. Phys. J. B")
    ("JPC" "The Journal of Physical Chemistry" "J. Phys. Chem.")
    ("JPCB" "The Journal of Physical Chemistry  B" "J. Phys. Chem. B")
    ("JPCC" "The Journal of Physical Chemistry C" "J. Phys. Chem. C")
    ("JCP" "The Journal of Chemical Physics" "J. Chem. Phys.")
    ("TSF" "Thin Solid Films" "Thin Solid Films")
    ("TC" "Topics in Catalysis" "Top. Catal.")
    ("WR" "Water Research" "Water Res.")

    ("AJCR" "American journal of cancer research" "Am. J. Cancer Res.")
    ("ACAMC" "Anti-Cancer Agents in Medicinal Chemistry" "Anti-Cancer Agents Med. Chem.")
    ("ACTPHARM" "Acta Pharmaceutica" "Acta Pharm.")
    ("ARCHPHARM" "Archiv der Pharmazie" "Arch. Pharm.")
    ("ACHEMBIO" "ACS Chemical Biology" "ACS Chem. Biol.")
    ("APTS" "ACS Pharmacology &amp; Translational Science" "ACS Pharmacol. Transl. Sci.")
    ("ACSOmega" "ACS Omega" "ACS Omega")
    ("ANNONC" "Annals of Oncology" "Ann. Oncol.")
    ("APOC" "Applied Organometallic Chemistry" "Appl. Organomet. Chem.")
    ("APSCI" "Applied Sciences" "Appl. Sci.-Basel")
    ("BIOORGCHEM" "Bioorganic Chemistry" "Bioorganic Chem.")
    ("BMC" "Bioorganic &amp; Medicinal Chemistry" "Bioorg. Med. Chem.")
    ("BMCL" "Bioorganic &amp; Medicinal Chemistry Letters" "Bioorg. Med. Chem. Lett.")
    ("BBR" "Biochemistry and Biophysics Reports" "Biochem. Biophys. Rep.")
    ("BBRC" "Biochemical and Biophysical Research Communications" "Biochem. Biophys. Res. Commun.")
    ("BCHEMAPP" "Bioinorganic Chemistry and Applications" "Bioinorg. Chem. Appl.")
    ("BIOCHEMPHARMA" "Biochemical Pharmacology" "Biochem. Pharmacol.")
    ("BRIEFBIOINF" "Briefings in Bioinformatics" "Brief. Bioinform.")
    ("BJC" "British Journal of Cancer" "Br. J. Cancer")
    ("BMJCR" "BMJ Case Reports" "BMJ Case Rep.")
    ("CANIVEST" "Cancer Investigation" "Cancer Invest.")
    ("CANLET" "Cancer Letters" "Cancer Lett.")
    ("CANRES" "Cancer Research" "Cancer Res.")
    ("CANCHEMPHARMA" "Cancer Chemotherapy and Pharmacology" "Cancer Chemother. Pharmacol.")
    ("CDD" "Cell Death Discovery" "Cell Death Discov.")
    ("CELLRES" "Cell Research" "Cell Res.")
    ("CHEMBIOD" "Chemistry &amp; Biodiversity" "Chem. Biodivers.")
    ("CHEMBIODD" "Chemical Biology &amp; Drug Design" "Chem. Biol. Drug Des.")
    ("CBI" "Chemico-Biological Interactions" "Chem.-Biol. Interact.")
    ("CLINCANRES" "Clinical Cancer Research" "Clin. Cancer Res.")
    ("COLSUFBIO" "Colloids and Surfaces B: Biointerfaces" "Colloid Surf. B-Biointerfaces")
    ("CURRCOMPDD" "Current Computer-Aided Drug Design" "Curr. Comput.-Aided Drug Des.")
    ("CURRORGSYNT" "Current Organic Synthesis" "Curr. Org. Synth.")
    ("CURRMEDCHEM" "Current Medicinal Chemistry" "Curr. Med. Chem.")
    ("CURRMOLMED" "Current Molecular Medicine" "Curr. Mol. Med.")
    ("CURRONCOREP" "Current Oncology Reports" "Curr. Oncol. Rep.")
    ("CURRTOPMEDCHEM" "Current Topics in Medicinal Chemistry" "Curr. Top. Med. Chem.")
    ("DALTTRANS" "Dalton Transactions" "Dalton Trans.")
    ("DDTODAY" "Drug Discovery Today" "Drug Discov. Today")
    ("DDDEVT" "Drug Design, Development and Therapy" "Drug Des. Dev. Ther.")
    ("DMP" "Drug Metabolism and Disposition" "Drug Metab. Dispos.")
    ("EJDMETPK" "European Journal of Drug Metabolism and Pharmacokinetics" "Eur. J. Drug Metabol. Pharmacokinet.")
    ("EJMEDCHEM" "European Journal of Medicinal Chemistry" "Eur. J. Med. Chem.")
    ("EJMEDCHEMR" "European Journal of Medicinal Chemistry Reports" "Eur. J. Med. Chem. Rep.")
    ("EJPHARMACOL" "European Journal of Pharmacology" "Eur. J. Pharmacol.")
    ("EJORGCHEM" "European Journal of Organic Chemistry" "Eur. J. Org. Chem.")
    ("EXPMOLMED" "Experimental &amp; Molecular Medicine" "Exp. Mol. Med.")
    ("EXPODD" "Expert Opinion on Drug Discovery" "Expert. Opin. Drug Discov.")
    ("EXOID" "Expert Opinion on Investigational Drugs" "Expert Opin. Investig. Drugs")
    ("EXPOTP" "Expert Opinion on Therapeutic Patents" "Expert Opin. Ther. Patents")
    ("EXPRAT" "Expert Review of Anticancer Therapy" "Expert Rev. Anticancer Ther")
    ("EXPRCP" "Expert Review of Clinical Pharmacology" "Expert Rev. Clin. Pharmacol.")
    ("EJC" "European Journal of Cancer" "Eur. J. Cancer")
    ("FRONTCHEM" "Frontiers in Chemistry" "Front. Chem.")
    ("FRONTMOLBIO" "Frontiers in Molecular Biosciences" "Front. Mol. Biosci.")
    ("FRONTPHARMACO" "Frontiers in Pharmacology" "Front. Pharmacol.")
    ("FUTMEDCHEM" "Future Medicinal Chemistry" "Future Med. Chem.")
    ("INORGCHEM" "Inorganic Chemistry" "Inorg. Chem.")
    ("IND" "Investigational New Drugs" "Invest. New Drugs")
    ("IJC" "International Journal of Cancer" "Int. J. Cancer")
    ("IJLSPR" "International Journal of Life Science and Pharma Research" "Int. J. Life Sci. Pharma Res.")
    ("IJMS" "International Journal of Molecular Sciences" "Int. J. Mol. Sci.")
    ("IJO" "International Journal of Oncology" "Int. J. Oncol.")
    ("ICA" "Inorganica Chimica Acta" "Inorg. Chim. Acta")
    ("JCOPO" "JCO Precision Oncology" "JCO Precis. Oncol.")
    ("JMEDCHEM" "Journal of Medicinal Chemistry" "J. Med. Chem.")
    ("IRJPAC" "International Research Journal of Pure and Applied Chemistry" "Int. Res. J. Pure Appl. Chem.")
    ("JBIC" "Journal of Biological Inorganic Chemistry" "J. Biol. Inorg. Chem.")
    ("JBSD" "Journal of Biomolecular Structure and Dynamics" "J. Biomol. Struct. Dyn.")
    ("JBO" "Journal of Bone Oncology" "J. Bone Oncol.")
    ("JCRCO" "Journal of Cancer Research and Clinical Oncology" "J. Cancer Res. Clin. Oncol.")
    ("JCELLBIO" "Journal of Cellular Biochemistry" "J. Cell. Biochem.")
    ("JCHEM" "Journal of Chemistry" "J. Chem.")
    ("JCIM" "Journal of Chemical Information and Modeling" "J. Chem Inf. Model.")
    ("JCONCO" "Journal of Clinical Oncology" "J. Clin. Oncol.")
    ("JCLINPATHO" "Journal of Clinical Pathology" "J. Clin. Pathol.")
    ("JCOORCHEM" "Journal of Coordination Chemistry" "J. Coord. Chem.")
    ("JEIMC" "Journal of Enzyme Inhibition and Medicinal Chemistry" "J. Enzym. Inhib. Med. Chem.")
    ("JECCR" "Journal of Experimental &amp; Clinical Cancer Research" "J. Exp. Clin. Cancer Res.")
    ("JETO" "Journal of Experimental Therapeutics and Oncology" "J. Exp. Ther. Oncol.")
    ("JICHEMSOC" "Journal of the Indian Chemical Society" "J. Indian Chem. Soc.")
    ("JINORGBIOCHEM" "Journal of Inorganic Biochemistry" "J. Inorg. Biochem.")
    ("JHETCHEM" "Journal of Heterocyclic Chemistry" "J. Heterocycl. Chem.")
    ("JNEURONC" "Journal of Neuro-Oncology" "J. Neuro-Oncol.")
    ("JONCPHARP" "Journal of Oncology Pharmacy Practice" "J. Oncol. Pharm. Pract.")
    ("JPS" "Journal of Pharmaceutical Sciences" "J. Pharm. Sci.")
    ("JPP" "Journal of Pharmacy and Pharmacology" "J. Pharm. Pharmacol.")
    ("JPET" "Journal of Pharmacology and Experimental Therapeutics" "J. Pharmacol. Exp. Ther.")
    ("JPBA" "Journal of Pharmaceutical and Biomedical Analysis" "J. Pharm. Biomed. Anal.")
    ("JPP" "Journal of Physiology and Pharmacology" "J. Physiol. Pharmacol.")
    ("JNCI" "JNCI Journal of the National Cancer Institute" "JNCI-J. Natl. Cancer Inst.")
    ("JOSS" "Journal of Open Source Software" "J. Open Source Softw.")
    ("JORGCHEM" "The Journal of Organic Chemistry" "J. Org. Chem.")
    ("JMOLSTRUC" "Journal of Molecular Structure" "J. Mol. Struct.")
    ("LANCETHEMATO" "The Lancet Haematology" "Lancet Haematol.")
    ("MATERTODPROC" "Materials Today: Proceedings" "Mater. Today Proc.")
    ("MEDCHEM" "Medicinal Chemistry" "Med. Chem.")
    ("MEDCHEMRES" "Medicinal Chemistry Research" "Med. Chem. Res.")
    ("MINIREVMEDCHEM" "Mini-Reviews in Medicinal Chemistry" "Mini-Rev. Med. Chem.")
    ("MICROBIOLSPEC" "Microbiology Spectrum" "Microbiol. Spectr.")
    ("Molecules" "Molecules" "Molecules")
    ("MOLCAN" "Molecular Cancer" "Mol. Cancer")
    ("MOLCAR" "Molecular Carcinogenesis" "Mol. Carcinog.")
    ("MOLCALTHER" "Molecular Cancer Therapeutics" "Mol. Cancer Ther.")
    ("MOLDIV" "Molecular Diversity" "Mol. Divers.")
    ("MOLMED" "Molecular Medicine" "Mol. Med.")
    ("MOLPHARM" "Molecular Pharmaceutics" "Mol. Pharm.")
    ("MOLPHARMACO" "Molecular Pharmacology" "Mol. Pharmacol.")
    ("NATMAT" "Nature Materials" "Nat. Mater.")
    ("NATMED" "Nature Medicine" "Nat. Med.")
    ("NATREVDD" "Nature Reviews Drug Discovery" "Nat. Rev. Drug Discov.")
    ("NATREVMCB" "Nature Reviews Molecular Cell Biology" "Nat. Rev. Mol. Cell Biol.")
    ("NATREVCAN" "Nature Reviews Cancer" "Nat. Rev. Cancer")
    ("NATCHEMBIO" "Nature Chemical Biology" "Nat. Chem. Biol.")
    ("NEJMED" "New England Journal of Medicine" "N. Engl. J. Med.")
    ("NJCHEM" "New Journal of Chemistry" "New J. Chem.")
    ("NUCACRES" "Nucleic Acids Research" "Nucleic Acids Res.")
    ("OBIOCHEM" "Organic &amp; Biomolecular Chemistry" "Org. Biomol. Chem.")
    ("ONCOREP" "Oncology Reports" "Oncol. Rep.")
    ("PBC" "Pediatric Blood &amp; Cancer" "Pediatr. Blood Cancer")
    ("Pharmaceuticals" "Pharmaceuticals" "Pharmaceuticals")
    ("PCJ" "Pharmaceutical Chemistry Journal" "Pharm. Chem. J.")
    ("PHARMACOLRES" "Pharmacological Research" "Pharmacol. Res.")
    ("PHARMACOLTHER" "Pharmacology &amp; Therapeutics" "Pharmacol. Ther.")
    ("PCPB" "Photochemistry and Photobiology" "Photochem. Photobiol.")
    ("PLOSONE" "PLoS One" "PLoS One")
    ("PAROC" "Polycyclic Aromatic Compounds" "Polycycl. Aromat. Compd.")
    ("PNAS" "Proceedings of the National Academy of Sciences" "PNAS")
    ("PBMB" "Progress in Biophysics and Molecular Biology" "Prog. Biophys. Mol. Biol.")
    ("RADIOONC" "Radiotherapy and Oncology" "Radiother. Oncol.")
    ("RADIAONC" "Radiation Oncology" "Radiat. Oncol.")
    ("RSCA" "RSC Advances" "RSC Adv.")
    ("RJGC" "Russian Journal of General Chemistry" "Russ. J. Gen. Chem.")
    ("RJBC" "Russian Journal of Bioorganic Chemistry" "Russ. J. Bioorg. Chem.")
    ("RPS" "Research in Pharmaceutical Sciences" "Res. Pharm. Sci.")
    ("RVS" "Research in Veterinary Science" "Res. Vet. Sci.")
    ("SAPAMBS" "Spectrochimica Acta Part A: Molecular and Biomolecular Spectroscopy" "Spectroc. Acta Pt. A-Molec. Biomolec. Spectr.")
    ("SCIREP" "Scientific Reports" "Sci Rep")
    ("STTT" "Signal Transduction and Targeted Therapy" "Signal Transduct. Target. Ther.")
    ("SYNTCOM" "Synthetic Communications" "Synth. Commun.")
    ("THERADMEDONCOL" "Therapeutic Advances in Medical Oncology" "Ther. Adv. Med. Oncol.")
    ("TETLETT" "Tetrahedron Letters" "Tetrahedron Lett.")
    ("TPS" "Trends in Pharmacological Sciences" "Trends Pharmacol. Sci.")
    )
  "List of (string journal-full-name journal-abbreviation)")

(defun jmax-bibtex-generate-longtitles ()
  (interactive)
  (with-temp-file "longtitles.bib"
    (dolist (row bibtex-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
                      (nth 0 row)
                      (nth 1 row))))))

(defun jmax-bibtex-generate-shorttitles ()
  (interactive)
  (with-temp-file "shorttitles.bib"
    (dolist (row bibtex-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
                      (nth 0 row)
                      (nth 2 row))))))

(defun jmax-stringify-journal-name (&optional key start end)
  "replace journal name with a string. The strings are defined in `bibtex-abbreviations'."
  (interactive)
  (bibtex-beginning-of-entry)
  (when
      (string= "article"
               (downcase
                (cdr (assoc "=type=" (bibtex-parse-entry)))))
    (let* ((full-names (mapcar
                        (lambda (row)
                          (cons  (nth 1 row) (nth 0 row)))
                        bibtex-abbreviations))
           (abbrev-names (mapcar
                          (lambda (row)
                            (cons  (nth 2 row) (nth 0 row)))
                          bibtex-abbreviations))
           (journal (s-trim (bibtex-autokey-get-field "journal")))
           (bstring (or
                     (cdr (assoc journal full-names))
                     (cdr (assoc journal abbrev-names)))))
      (when bstring
        (bibtex-set-field "journal" bstring t)
        (bibtex-fill-entry)))))

(bibtex-map-entries 'jmax-stringify-journal-name)

(setq org-latex-image-default-width nil)

;;				Last Update HTML
(defun my-org-html-postamble (plist)
 (format "Last update : %s" (format-time-string "%d %b %Y")))
(setq org-html-postamble 'my-org-html-postamble)

(setq org-file-apps
   '(
     ("\\.docx\\'" . default)
     ("\\.pptx\\'" . default)
     ))

(defun evil-normalize-all-buffers ()
  "Force a drop to normal state."
  (unless (eq evil-state 'normal)
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (unless (or (minibufferp)
                  (eq evil-state 'emacs))
        (evil-force-normal-state)))
    (message "Dropped back to normal state in all buffers")))

(defvar evil-normal-timer
  (run-with-idle-timer 10 t #'evil-normalize-all-buffers)
  "Drop back to normal state after idle for 30 seconds.")

(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (call-interactively #'save-buffer)))

(with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "s-i") 'evil-normal-state))

(add-hook 'save-buffer
          (lambda ()
            (call-interactively #'evil-insert-state-exit-hook)))

(with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "s-s") 'evil-normal-state))
