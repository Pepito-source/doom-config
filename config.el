  (setq user-full-name "Vincent Montero"
        user-mail-address "vincent_montero@icloud.com")

(setq my/home-dir "/Users/vincentmontero/")

(setq my/sync-base-dir (concat my/home-dir "/Library/Mobile Documents/com~apple~CloudDocs/"))
(setq my/work-base-dir (concat my/sync-base-dir "02_work/"))
(setq my/media-base-dir (concat my/sync-base-dir "Media/"))
(setq org-directory my/work-base-dir
      org-roam-directory    (concat org-directory "org-roam/")
      )

(setq scimax-dir "~/scimax/")
(setq scimax-user-dir "~/scimax/")

(setq python-shell-interpreter "/opt/homebrew/anaconda3/bin/python")

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

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

(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmd! (if (org-on-heading-p)
                         (org-backward-element)
                       (evil-previous-visual-line)))
       :n "gj" (cmd! (if (org-on-heading-p)
                         (org-forward-element)
                       (evil-next-visual-line)))))

(after! dired
  (setq dired-listing-switches "-alhv"
        delete-by-moving-to-trash t)

  (map!
   :map dired-mode-map
   :n [DEL] #'dired-up-directory)
  )

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

(add-to-list 'load-path "/opt/homebrew/Cellar/mu/1.8.14/share/emacs/site-lisp/mu/mu4e")
;; (require 'mu4e)
(require 'smtpmail)

(require 'cc-mode)

(use-package f)
(use-package diminish)
(use-package lispy)
(use-package aggressive-indent)
(use-package ibuffer-projectile)
(use-package ivy-yasnippet
  :bind ("H-," . ivy-yasnippet))
(use-package ess-smart-equals)

(add-to-list 'load-path "~/scimax")

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
(setq scimax-journal-root-dir (concat my/work-base-dir "journal"))

(easy-menu-add)
(eval-after-load 'easy-menu
  '(progn
     (require 'scimax-notebook)))

(require 'scimax-ivy) ; M-TAB for multiple selection
(require 'scimax-yas)
(require 'scimax-elfeed)
(require 'scimax-spellcheck)

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


(let ((langs '("british" "english" "french" "spanish")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))
(global-set-key (kbd "H-m") 'cycle-ispell-languages)

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

(defun my/org-mode-IC_{50}-autoformat ()
  "Autoformat IC_{50} as IC_{50} in Org-mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "IC_{50}" nil t)
      (replace-match "IC_{50}"))))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'my/org-mode-IC_{50}-autoformat nil t)))

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

(require 'citar)
(require 'citar-org-roam)

(defvar bibtex-completion-edit-notes-function #'citar-open-notes
  "Function used to edit notes.
The function should accept one argument, a list of BibTeX keys.")

(defun bibtex-completion-edit-notes (keys)
  "Open the notes associated with KEYS using `bibtex-completion-edit-notes-function'."
  (funcall bibtex-completion-edit-notes-function keys))

(defconst my/bib-libraries
   (directory-files "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-entries/" t "\\.bib$")
   ) ; All of my bib databases.

(defconst my/main-pdfs-library-path
  '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-pdfs/")) ; Main PDFs directory

(defconst my/bib-notes-dir "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/org-roam/references/") ; I use org-roam to manage all my notes, including bib notes.

(defconst my/csl-dir "~/Library/Mobile Documents/com~apple~CloudDocs/02_work/csl/")

(use-package! citar
  :hook (doom-after-init-modules . citar-refresh)
  :config
  (require 'citar-org)
  :custom
  (org-cite-global-bibliography my/bib-libraries)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)

  (citar-bibliography my/bib-libraries)
  (citar-library-paths my/main-pdfs-library-path)
  (citar-notes-paths my/bib-notes-dir)

  (citar-citeproc-csl-styles-dir my/csl-dir)

  (citar-templates
   '((main . "${author editor:30}   ${date year issued:4}    ${title:110}")
     (suffix . "     ${=type=:20}    ${tags keywords keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "#+title: Notes on ${author editor}, ${title}") ; For new notes
     ))
  ;; Configuring all-the-icons. From
  ;; https://github.com/bdarcus/citar#rich-ui
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-green :v-adjust -0.1) .
      ,(all-the-icons-faicon "file-pdf-o" :face 'kb/citar-icon-dim :v-adjust -0.1) )
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) .
           ,(all-the-icons-material "speaker_notes" :face 'kb/citar-icon-dim :v-adjust -0.3))
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) .
           ,(all-the-icons-octicon "link" :face 'kb/citar-icon-dim :v-adjust 0.01))))
  (citar-symbol-separator "  ")
  :init
  ;; Here we define a face to dim non 'active' icons, but preserve alignment.
  ;; Change to your own theme's background(s)
  (defface kb/citar-icon-dim
    ;; Change these colors to match your theme. Using something like
    ;; `face-attribute' to get the value of a particular attribute of a face might
    ;; be more convenient.
    '((((background dark)) :foreground "#212428")
      (((background light)) :foreground "#f0f0f0"))
    "Face for having icons' color be identical to the theme
  background when \"not shown\".")

  )

(setq citar-org-roam-note-title-template
      (concat
       "${author} - ${title}\n"
       "#+roam_key: cite:${=key=}\n"
       "* ${title}\n"
       ":PROPERTIES:\n"
       ":Custom_ID: ${=key=}\n"
       ":NOTER_DOCUMENT: [[~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-pdfs/${=key=}.pdf]]\n"
       ":AUTHOR: ${author-abbrev}\n"
       ":JOURNAL: ${journaltitle}\n"
       ":DATE: ${date}\n"
       ":YEAR: ${year}\n"
       ":DOI: ${doi}\n"
       ":URL: ${url}\n"
       ":END:\n\n")
      )

(use-package! oc-bibtex :after oc)

(use-package! ivy-bibtex
  :when (modulep! :completion vertico)
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  (setq bibtex-completion-additional-search-fields '(("journaltitle")
                                                     (keywords))
        bibtex-completion-pdf-symbol ""
        bibtex-completion-notes-symbol ""
        bibtex-completion-pdf-field "file"
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}"))

        )
  )

(setq bibtex-completion-notes-path my/bib-notes-dir
      bibtex-completion-bibliography my/bib-libraries
      bibtex-completion-library-path my/main-pdfs-library-path
      bibtex-completion-pdf-field "file"
      ;; bibtex-completion-notes-template-multiple-files
      ;; (concat
      ;;  "#+title: ${title}\n"
      ;;  "#+roam_key: cite:${=key=}\n"
      ;;  "* ${title}\n"
      ;;  ":PROPERTIES:\n"
      ;;  ":Custom_ID: ${=key=}\n"
      ;;  ":NOTER_DOCUMENT: [[~/Library/Mobile Documents/com~apple~CloudDocs/02_work/bibtex-pdfs/${=key=}.pdf]]\n"
      ;;  ":AUTHOR: ${author-abbrev}\n"
      ;;  ":JOURNAL: ${journaltitle}\n"
      ;;  ":DATE: ${date}\n"
      ;;  ":YEAR: ${year}\n"
      ;;  ":DOI: ${doi}\n"
      ;;  ":URL: ${url}\n"
      ;;  ":END:\n\n")
      )


;; Opening PDF files outside emacs, by default PDFs open in PDFTools
;; (setq! bibtex-completion-pdf-open-function  (lambda (fpath)
;;                                            (call-process "open" nil 0 nil fpath))
;;      )

(use-package! org-ref
  :after org
  :config
  (require 'org-ref-ivy)
  )

(defun DDG-this ()
  "Perform a DuckDuckGo search for the selected text or prompt for input."
  (interactive)
  (let ((search-term (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-string "Search: "))))
    (browse-url (format "https://duckduckgo.com/?q=%s" (url-hexify-string search-term)))))

(setq bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator "-"
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 2
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5)

(defun scifinder ()
  "Open https://scifinder.cas.org/scifinder/view/scifinder/scifinderExplore.jsf in a browser."
  (interactive)
  (browse-url   "https://sso-cas-org.lama.univ-amu.fr/as/authorization.oauth2?response_type=code&client_id=scifinder-n&redirect_uri=https%3A%2F%2Fscifinder-n.cas.org%2Fpa%2Foidc%2Fcb&state=eyJ6aXAiOiJERUYiLCJhbGciOiJkaXIiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2Iiwia2lkIjoicjVEZU5XUTN6TENDNERTdjR6ZDFxakc5eDRZIiwic3VmZml4IjoibWlGS09lLjE2ODcwODI3NzQifQ..gqelHLndPNYd13nUhUc0yg.qq1i9fRNWSb5CK-VzClHUvjp9DZ9hxIR-WMamA9Phg1Ee--s_n0OV_PiUVtFPuESKYKDd2onqlb11tO5qPLP7A.n2MA2ZFpH2NKU1Mvf8ubTA&nonce=FYiL2oWWopZxgdQwskEt7dxmpJ2Gb9KF_c_jSE80I3g&scope=openid%20address%20email%20phone%20profile&vnd_pi_requested_resource=https%3A%2F%2Fscifinder-n.cas.org%2F&vnd_pi_application_name=SciFinder-nIDF"))

(defun pubmed ()
  "Open http://www.ncbi.nlm.nih.gov/pubmed in a browser."
  (interactive)
  (browse-url "https://pubmed.ncbi.nlm.nih.gov/?otool=iframuscdlib"))

(defun bu-amu ()
  "Open http://www.ncbi.nlm.nih.gov/pubmed in a browser."
  (interactive)
  (browse-url "https://univ-amu.summon.serialssolutions.com/#!/search?ho=t&include.ft.matches=f&l=fr-FR&q="))

(defun chatGPT ()
  "Open http://www.ncbi.nlm.nih.gov/pubmed in a browser."
  (interactive)
  (browse-url "https://chat.openai.com"))

(define-key org-mode-map (kbd "s-)") 'org-ref-insert-link)
(define-key org-mode-map (kbd "s-(") 'org-ref-insert-link-hydra/body)
(define-key org-mode-map (kbd "s-à") 'org-ref-insert-ref-link)
(define-key org-mode-map (kbd "s-ç") 'org-ref-insert-label-link)
(define-key bibtex-mode-map (kbd "H-p") 'org-ref-bibtex-hydra/body)

;; * Doom emacs keybinding for inserting org ref link to bibtex entry
(map! :leader
      ;; Inserting
      :desc "Insert citation"
      "i i" #'org-ref-insert-link
      :desc "Insert label"
      "i l" #'org-ref-insert-ref-link
      :desc "DOI Add bibtex entry"
      "i d" #'doi-add-bibtex-entry

      ;; Opening
      :desc "Scifinder"
      "o s" #'scifinder
      :desc "Pubmed"
      "o m" #'pubmed
      :desc "BU AMU"
      "o B" #'bu-amu
      :desc "Chat GPT"
      "o c" #'chatGPT

      ;; Searching
      :desc "Google this"
      "s g" #'google-this
      :desc "DuckDuckGo this"
      "s @" #'DDG-this

      :desc "ORB Actions"
      "n r @" #'orb-note-actions

      )

(use-package org-mac-link)

(eval-after-load 'org '(require 'org-pdfview))

(add-to-list 'org-file-apps
             '("\\.pdf\\'" . (lambda (file link)
                                     (org-pdfview-open link))))

(setq
 org-noter-notes-search-path '("~/Library/Mobile Documents/com~apple~CloudDocs/02_work/org-roam/references")
 )

(use-package! org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "No PDF found for %s" key))))
(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

(add-hook 'pdf-tools-enabled-hook 'pdf-view-dark-minor-mode)

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

(defun org-cite-list-bibliography-files ()
  "List all bibliography files defined in the buffer."
  (delete-dups
   (append (mapcar (lambda (value)
		     (pcase value
		       (`(,f . ,d)
                        (setq f (org-strip-quotes f))
                        (if (or (file-name-absolute-p f)
                                (file-remote-p f)
                                (equal d default-directory))
                            ;; Keep absolute paths, remote paths, and
                            ;; local relative paths.
                            f
                          ;; Adjust relative bibliography path for
                          ;; #+SETUP files located in other directory.
                          ;; Also, see `org-export--update-included-link'.
                          (file-relative-name
                           (expand-file-name f d) default-directory)))))
		   (pcase (org-collect-keywords
                           '("BIBLIOGRAPHY") nil '("BIBLIOGRAPHY"))
		     (`(("BIBLIOGRAPHY" . ,pairs)) pairs)))
	   org-cite-global-bibliography)))

(setq org-latex-title-command "")

(setq org-latex-prefer-user-labels t)

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "bibtex $(basename %b)"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")
      )

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
(add-to-list 'org-latex-packages-alist '("" "longtable" nil))             ; Tabulars with adjustable-width columns
(add-to-list 'org-latex-packages-alist '("" "booktabs" t))  ; for \toprule etc. in tables
(add-to-list 'org-latex-packages-alist '("" "multirow" nil))        ; Create tabular cells spanning multiple rows
(add-to-list 'org-latex-packages-alist '("" "array" nil))      ; For table array
(add-to-list 'org-latex-packages-alist '("" "xcolor, colortbl" nil)) ; To provide color for soul (for english editing), for adding cell color of table
(setq org-latex-tables-booktabs t)

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

(add-to-list 'org-latex-packages-alist '("" "glossaries" nil))
(add-to-list 'org-latex-packages-alist '("" "makeidx" nil))

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

     (add-to-list 'org-latex-classes
                  '("jmedchem"
                    "\\documentclass{achemso}
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     [EXTRA]"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" "\\newpage" "\\subsection*{%s}" "\\newpage")
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
                    ("\\subsection{%s}" "\\newpage" "\\subsection*{%s}" "\\newpage")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                    ))

     )

(defun my-org-export-to-pdf-gloss-bibtex ()
  "Export the current buffer to PDF using Org mode and open the resulting PDF file."
  (interactive)
  (let ((org-export-before-parsing-hook '(org-ref-glossary-before-parsing
                                           org-ref-acronyms-before-parsing))
        (org-latex-pdf-process
         '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "bibtex %b"
           "makeglossaries %b"
           "makeindex %b"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")))
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
           "makeglossaries %b"
           "makeindex %b"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")))
    (org-latex-export-to-pdf)
    (org-open-file (concat (file-name-sans-extension buffer-file-name) ".pdf"))))

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

(setq org-latex-image-default-width nil)

;;				Last Update HTML
(defun my-org-html-postamble (plist)
 (format "Last update : %s" (format-time-string "%d %b %Y")))
(setq org-html-postamble 'my-org-html-postamble)
