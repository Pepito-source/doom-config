(setq user-full-name "Vincent Montero"
      user-mail-address "vincent_montero@icloud.com")

(setq my/home-dir "/Users/vincentmontero/")

(setq my/sync-base-dir (concat my/home-dir "/Library/Mobile Documents/com~apple~CloudDocs/"))
(setq my/work-base-dir (concat my/sync-base-dir "02_work/"))
(setq my/media-base-dir (concat my/sync-base-dir "Media/"))
(setq org-directory my/work-base-dir
      org-roam-directory    (concat org-directory "org-roam/"))

(setq scimax-dir "~/scimax/.emacs.d")
(setq scimax-user-dir "~/scimax/")

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.8.14/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)
(require 'smtpmail)

(use-package f)
(use-package diminish)
(use-package lispy)

(add-to-list 'load-path "~/scimax")
(require 'scimax-hydra)



(require 'ob-jupyter)
(require 'scimax-jupyter)
(require 'scimax)
(require 'scimax-mode)
(require 'scimax-org)
(require 'scimax-utils)
(require 'scimax-spellcheck)

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
  (run-with-idle-timer 30 t #'evil-normalize-all-buffers)
  "Drop back to normal state after idle for 30 seconds.")

(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (call-interactively #'save-buffer)))

(beacon-mode 1)

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

(let ((langs '("spanish" "british" "french" "english")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))


(provide 'vm-aspell)
(require 'vm-aspell)

(use-package flycheck
  ;; Jun 28 - I like this idea, but sometimes this is too slow.
  :config
  (add-hook 'text-mode-hook #'flycheck-mode)
  (add-hook 'org-mode-hook #'flycheck-mode)
  (define-key flycheck-mode-map (kbd "s-;") 'flycheck-previous-error))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org")))

(use-package org-ref
    :after org
    :init
    ; code to run before loading org-ref
    :config
    ; code to run after loading org-ref
    )

;(require 'openalex)

(define-key org-mode-map (kbd "s-)") 'org-ref-insert-link)
(define-key org-mode-map (kbd "s-(") 'org-ref-insert-link-hydra/body)
(define-key org-mode-map (kbd "s-à") 'org-ref-insert-ref-link)
(define-key org-mode-map (kbd "s-ç") 'org-ref-insert-label-link)
(define-key bibtex-mode-map (kbd "H-p") 'org-ref-bibtex-hydra/body)

(use-package org-ref-ivy
  :ensure nil
  :load-path (lambda () (expand-file-name "org-ref" scimax-dir))
  :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
	      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
	      org-ref-insert-label-function 'org-ref-insert-label-link
	      org-ref-insert-ref-function 'org-ref-insert-ref-link
	      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))

(defun org-markup-region-or-point (type beginning-marker end-marker)
  "Apply the markup TYPE with BEGINNING-MARKER and END-MARKER to region, word or point.
This is a generic function used to apply markups. It is mostly
the same for the markups, but there are some special cases for
subscripts and superscripts."
  (cond
   ;; We have an active region we want to apply
   ((region-active-p)
    (let* ((bounds (list (region-beginning) (region-end)))
	   (start (apply 'min bounds))
	   (end (apply 'max bounds))
	   (lines))
      ;; set some bounds here, unless it is a subscript/superscript
      ;; Those start at point or region
      (unless (memq type '(subscript superscript))
	(save-excursion
	  (goto-char start)
	  (unless (looking-at " \\|\\<")
	    (backward-word)
	    (setq start (point)))
	  (goto-char end)
	  (unless (or (looking-at " \\|\\>")
		      (looking-back "\\>" 1))
	    (forward-word)
	    (setq end (point)))))

      (setq lines
	    (s-join "\n" (mapcar
			  (lambda (s)
			    (if (not (string= (s-trim s) ""))
				(concat beginning-marker
					(s-trim s)
					end-marker)
			      s))
			  (split-string
			   (buffer-substring start end) "\n"))))
      (setf (buffer-substring start end) lines)
      (forward-char (length lines))))
   ;; We are on a word with no region selected
   ((thing-at-point 'word)
    (cond
     ;; beginning of a word
     ((looking-back " " 1)
      (insert beginning-marker)
      (re-search-forward "\\>")
      (insert end-marker))
     ;; end of a word
     ((looking-back "\\>" 1)
      (insert (concat beginning-marker end-marker))
      (backward-char (length end-marker)))

     ;; looking back at closing char
     ((and (memq type '(subscript superscript))
	   (looking-back end-marker 1))
      (delete-char -1)
      (forward-char)
      (insert end-marker))

     ;; not at start or end so we just sub/sup the character at point
     ((memq type '(subscript superscript))
      (insert beginning-marker)
      (forward-char (- (length beginning-marker) 1))
      (insert end-marker))
     ;; somewhere else in a word and handled sub/sup. mark up the
     ;; whole word.
     (t
      (re-search-backward "\\<")
      (insert beginning-marker)
      (re-search-forward "\\>")
      (insert end-marker))))
   ;; looking back at end marker, slurp next word in
   ((looking-back end-marker (length end-marker))
    (delete-char (* -1 (length end-marker)))
    (forward-word)
    (insert end-marker))
   ;; not at a word or region insert markers and put point between
   ;; them.
   (t
    (insert (concat beginning-marker end-marker))
    (backward-char (length end-marker)))))


(defun org-double-quote-region-or-point ()
  "Double quote the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'italics "\"" "\""))


(defun org-single-quote-region-or-point ()
  "Single quote the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'italics "'" "'"))


(defun org-italics-region-or-point ()
  "Italicize the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'italics "/" "/"))


(defun org-bold-region-or-point ()
  "Bold the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'bold "*" "*"))


(defun org-underline-region-or-point ()
  "Underline the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'underline "_" "_"))


(defun org-code-region-or-point ()
  "Mark the region, word or character at point as code.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'underline "~" "~"))


(defun org-verbatim-region-or-point ()
  "Mark the region, word or character at point as verbatim.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'underline "=" "="))


(defun org-strikethrough-region-or-point ()
  "Mark the region, word or character at point as strikethrough.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'strikethrough "+" "+"))


(defun org-subscript-region-or-point ()
  "Mark the region, word or character at point as a subscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'subscript "_{" "}"))


(defun org-superscript-region-or-point ()
  "Mark the region, word or character at point as superscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'superscript "^{" "}"))


(defun org-latex-math-region-or-point (&optional arg)
  "Wrap the selected region in latex math markup.
\(\) or $$ (with prefix ARG) or @@latex:@@ with double prefix.
With no region selected, insert those and put point in the middle
to add an equation. Finally, if you are between these markers
then exit them."
  (interactive "P")
  (if (memq 'org-latex-and-related (get-char-property (point) 'face))
      ;; in a fragment, let's get out.
      (goto-char (or (next-single-property-change (point) 'face) (line-end-position)))
    (let ((chars
	   (cond
	    ((null arg)
	     '("\\(" . "\\)"))
	    ((equal arg '(4))
	     '("$" . "$"))
	    ((equal arg '(16))
	     '("@@latex:" . "@@")))))
      (if (region-active-p)
	  ;; wrap region
	  (progn
	    (goto-char (region-end))
	    (insert (cdr chars))
	    (goto-char (region-beginning))
	    (insert (car chars)))
	(cond
	 ((thing-at-point 'word)
	  (save-excursion
	    (end-of-thing 'word)
	    (insert (cdr chars)))
	  (save-excursion
	    (beginning-of-thing 'word)
	    (insert (car chars)))
	  (forward-char (length (car chars))))
	 ;; slurp next word if you call it again
	 ((and (not (equal arg '(16))) (looking-back (regexp-quote (cdr chars)) (length (cdr chars))))
	  (delete-char (* -1 (length (cdr chars))))
	  (forward-word)
	  (insert (cdr chars)))
	 (t
	  (insert (concat  (car chars) (cdr chars)))
	  (backward-char (length (cdr chars)))))))))


(defun ivy-insert-org-entity ()
  "Insert an org-entity using ivy."
  (interactive)
  (ivy-read "Entity: " (cl-loop for element in (append org-entities org-entities-user)
				when (not (stringp element))
				collect
				(cons
				 (format "%20s | %20s | %20s | %s"
					 (cl-first element)    ;name
					 (cl-second element)   ; latex
					 (cl-fourth element)   ; html
					 (cl-seventh element)) ;utf-8
				 element))
	    :require-match t
	    :action '(1
		      ("u" (lambda (candidate)
			     (insert (cl-seventh (cdr candidate)))) "utf-8")
		      ("o" (lambda (candidate)
			     (insert "\\" (cl-first (cdr candidate)))) "org-entity")
		      ("l" (lambda (candidate)
			     (insert (cl-second (cdr candidate)))) "latex")
		      ("h" (lambda (candidate)
			     (insert (cl-fourth (cdr candidate)))) "html")
		      ("a" (lambda (candidate)
			     (insert (cl-fifth (cdr candidate)))) "ascii")
		      ("L" (lambda (candidate)
			     (insert (cl-sixth (cdr candidate))) "Latin-1")))))

(use-package! org-gtd
  :after org
  :config
  ;; where org-gtd will put its files. This value is also the default one.
  (setq org-gtd-directory (concat my/work-base-dir "gtd/"))
  ;; package: https://github.com/Malabarba/org-agenda-property
  ;; this is so you can see who an item was delegated to in the agenda
  (setq org-agenda-property-list '("DELEGATED_TO"))
  ;; I think this makes the agenda easier to read
  (setq org-agenda-property-position 'next-line)
  ;; package: https://www.nongnu.org/org-edna-el/
  ;; org-edna is used to make sure that when a project task gets DONE,
  ;; the next TODO is automatically changed to NEXT.
  (setq org-edna-use-inheritance t)
  (org-edna-load)
  :bind
  (("C-c d c" . org-gtd-capture) ;; add item to inbox
   ("C-c d a" . org-agenda-list) ;; see what's on your plate today
   ("C-c d p" . org-gtd-process-inbox) ;; process entire inbox
   ("C-c d n" . org-gtd-show-all-next) ;; see all NEXT items
   ;; see projects that don't have a NEXT item
   ("C-c d s" . org-gtd-show-stuck-projects)
   ;; the keybinding to hit when you're done editing an item in the
   ;; processing phase
   ("C-c d f" . org-gtd-clarify-finalize)))

(use-package org-mac-link)

(defvar bib-dir "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/")

(use-package ivy-bibtex
  :init
  (setq bibtex-completion-bibliography (directory-files-recursively "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/" "\.bib$")
	bibtex-completion-library-path '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-pdfs/")
	bibtex-completion-notes-path "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/notes/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil fpath))))

(after! org
  (setq org-agenda-files (append
                       '("/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/douleur/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/pharmacometrie/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/stresam/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/cannapark/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/hopital/csh/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/assos/amipbm/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/assos/fnsipbm/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/perso/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/biology/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/chemistry/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/conseil-scientifique/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/communications/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/computer-science/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/funding/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/teaching/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/these-pharma/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/these-science/"
                          "/Users/vincentmontero/Library/Mobile Documents/com~apple~CloudDocs/02_work/univ/writing-articles/")
                        ))
)

(setq org-latex-title-command "")

(setq org-latex-prefer-user-labels t)

(setq org-latex-pdf-process
      (quote (
              "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
              "bibtex $(basename %b)"
              "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
              "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")))

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t)   ;; this is for having good fonts
        ("" "lmodern" nil)      ;; This is for handling accented characters
        ("T1" "fontenc" t)      ;; This makes standard margins
        ("top=1in, bottom=1.in, left=1in, right=1in" "geometry" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)	  ;makes it possible to wrap text around figures
        ("" "rotating" nil)
        ("normalem" "ulem" t)

        ;; These provide math symbols
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "amssymb" t)
        ("" "amsmath" t)
        ("theorems, skins" "tcolorbox" t)

        ;; used for marking up chemical formulars
        ("version=3" "mhchem" t)

        ;; bibliography
        ("numbers,super,sort&compress" "natbib" nil)
        ("" "natmove" nil)
        ("" "url" nil)

        ;; this is used for syntax highlighting of code
        ("cache=false" "minted" nil)

        ;; this allows you to use underscores in places like filenames. I still wouldn't do it.
        ("strings" "underscore" nil)
        ("linktocpage, pdfstartview=FitH, colorlinks, linkcolor=blue, anchorcolor=blue, citecolor=blue, filecolor=blue, menucolor=blue, urlcolor=blue"
         "hyperref" nil)

        ;; enables you to embed files in pdfs
        ("" "attachfile" nil)

        ;; set default spacing CONFLICT WITH BIBLATEX IN BEAMER
        ;;("" "setspace" nil)

))

(add-to-list 'org-latex-packages-alist '("" "indentfirst" nil))     ; Indent first paragraph after section header
(add-to-list 'org-latex-packages-alist '("right" "lineno" nil))          ; Line numbers on paragraphs
(add-to-list 'org-latex-packages-alist '("" "enumitem" nil))  ; Control layout of itemize, enumerate, description

(add-to-list 'org-latex-packages-alist '("" "soul" nil))             ; To highlight text
(add-to-list 'org-latex-packages-alist '("" "microtype" nil))       ; For command \textls[]{}

(add-to-list 'org-latex-packages-alist '("" "marginnote" nil))       ; For left column
(add-to-list 'org-latex-packages-alist '("" "marginfix" nil)) ; For command \clearmargin for manually moving the left column to the next page
(add-to-list 'org-latex-packages-alist '("" "fancyhdr" nil)) ; Extensive control of page headers and footers in LATEX2ε
(add-to-list 'org-latex-packages-alist '("" "lastpage" nil)) ; Reference last page for Page N of M type footers
(add-to-list 'org-latex-packages-alist '("" "etoolbox" nil))  ; for \AtBeginDocument etc.
(add-to-list 'org-latex-packages-alist '("" "tabto" nil))     ; To use tab for alignment on first page
(add-to-list 'org-latex-packages-alist '("" "totcount" nil)) ; To enable extracting the value of the counter "page"
(add-to-list 'org-latex-packages-alist '("" "ragged2e" nil))   ; For command \justifying
(add-to-list 'org-latex-packages-alist '("" "pbox" nil))       ; For biography environment
(add-to-list 'org-latex-packages-alist '("" "enotez" nil))    ; For endnotes

(add-to-list 'org-latex-packages-alist '("" "pdfpages" nil))           ; Include PDF documents in LATEX

(add-to-list 'org-latex-packages-alist '("" "adjustbox" t))
(add-to-list 'org-latex-packages-alist '("skip=0.5 \\baselineskip" "caption" nil)) ; Customising captions in floating environments

(add-to-list 'org-latex-packages-alist '("" "epstopdf" nil)) ; Convert EPS to PDF using Ghostscript
(add-to-list 'org-latex-packages-alist '("" "tikz" nil))            ; For \foreach used for Orcid icon
(add-to-list 'org-latex-packages-alist '("" "changepage" nil)) ; To adjust the width of the column for the title part and figures/tables (adjustwidth environment)
(add-to-list 'org-latex-packages-alist '("" "graphbox" nil)) ; To align graphics inside tables

(add-to-list 'org-latex-packages-alist '("" "tabularx" nil))             ; Tabulars with adjustable-width columns
(add-to-list 'org-latex-packages-alist '("" "booktabs" t))  ; for \toprule etc. in tables
(add-to-list 'org-latex-packages-alist '("" "multirow" nil))        ; Create tabular cells spanning multiple rows
(add-to-list 'org-latex-packages-alist '("" "array" nil))      ; For table array
(add-to-list 'org-latex-packages-alist '("" "xcolor, colortbl" nil)) ; To provide color for soul (for english editing), for adding cell color of table
(setq org-e-latex-tables-booktabs t)

(add-to-list 'org-latex-packages-alist '("" "calc" nil))            ; Simple arithmetic in LATEX commands
(add-to-list 'org-latex-packages-alist '("" "mathpazo" nil))  ; Fonts to typeset mathematics to match Palatino
(add-to-list 'org-latex-packages-alist '("" "upgreek" nil))    ; For making greek letters not italic
(add-to-list 'org-latex-packages-alist '("" "attrib" nil))     ; For XML2PDF use \tag{} for equation

(setq org-latex-listings 'minted)
(setq org-latex-custom-lang-environments
            '((emacs-lisp "common-lispcode")))

(add-to-list 'org-latex-packages-alist '("version=4" "mhchem" t)) ; provides commands for typesetting chemical molecular formulae and equations.
(add-to-list 'org-latex-packages-alist '("" "chemmacros" t)) ; A collection of macros to support typesetting chemistry documents, nomenclature commands, oxidation numbers, thermodynamic data, newman projections, etc.
(add-to-list 'org-latex-packages-alist '("" "textgreek" t)) ; Use upright greek letters as text symbols, e.g. \textbeta
(add-to-list 'org-latex-packages-alist '("" "chemnum" t))   ; A method for numbering chemical compounds
(add-to-list 'org-latex-packages-alist '("" "bpchem" t)) ;numbering molecules with \CNref
(add-to-list 'org-latex-packages-alist '("" "chemnum" t))

(add-to-list 'org-latex-packages-alist '("" "ifthen" nil)) ; Conditional commands in LATEX documents : The package’s basic command is \ifthenelse, which can use a wide array of tests

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
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     [EXTRA]"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" "\\newpage" "\\subsection*{%s}" "\\newpage")
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
                     [PACKAGES]
                     [EXTRA]"
                    ;; ("\\part{%s}" . "\\part*{%s}")
                    ("\\chapter{%s}" . "\\chapter*{%s}")
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ))
     )

;;				Last Update HTML
(defun my-org-html-postamble (plist)
 (format "Last update : %s" (format-time-string "%d %b %Y")))
(setq org-html-postamble 'my-org-html-postamble)

;; Meta key on apple keyboard
(setq ns-alternate-modifier 'meta)
(setq ns-right-alternate-modifier 'none)

;; set keys for Apple keyboard, for emacs in OS X
(setq mac-control-modifier 'control) ; make Control key do Control
(setq mac-option-modifier 'meta) ; make cmd left key do Meta
(setq mac-left-command-modifier 'super) ; make left opt key do Super
(setq mac-right-command-modifier 'hyper)  ; make cmd right key do Hyper

;;(global-set-key (kbd "H-f") 'toggle-evilmode)

(global-set-key (kbd "M-q") 'toggle-truncate-lines)

(global-set-key (kbd "H-k") 'windmove-up)
(global-set-key (kbd "H-j") 'windmove-down)
(global-set-key (kbd "H-l") 'windmove-right)
(global-set-key (kbd "H-h") 'windmove-left)

(with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "s-s") 'evil-normal-state))

(with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "s-i") 'evil-normal-state))

(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (call-interactively #'save-buffer)))

(add-hook 'save-buffer
          (lambda ()
            (call-interactively #'evil-insert-state-exit-hook)))

;; moving between windows with Shift + arrows
;; (windmove-default-keybindings)


(global-set-key (kbd "H-l") 'ns-copy-including-secondary)

;; * Doom emacs keybinding for inserting org ref link to bibtex entry
(map! :leader
      :desc "Org-ref insert link"
      "i i" #'org-ref-insert-link
      "i l" #'org-ref-insert-ref-link)

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

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

(add-hook 'pdf-tools-enabled-hook 'pdf-view-dark-minor-mode)
