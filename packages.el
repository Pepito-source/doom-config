;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! org-auto-tangle)              ;autotangling README.org to README.el

(package! beacon)

(package! ox-pandoc)

(package! ox-ipynb :recipe
  (:host github
   :repo "jkitchin/ox-ipynb"
   :files ("*.el")))

(package! org-ref :recipe
  (:host github
   :repo "jkitchin/org-ref"))

(package! scimax :recipe
  (:local-repo "~/scimax/.emacs.d/"))

(package! pretty-hydra)

(package! org-mac-link)

(package! ivy-bibtex)
(package! ivy-xref)
(package! ivy-yasnippet)

(package! counsel)

(package! google-this)

(package! org-download)

(package! org-gtd)

(package! org-pdfview)

(package! org-noter-pdftools)

(package! diminish) 
(package! aggressive-indent) 
(package! auto-complete) 
(package! button-lock) 
(package! multiple-cursors) 
(package! dashboard) 
(package! esup) 
(package! flx) 
(package! hy-mode) 
(package! mustache) 
(package! ov) 
(package! helm-bibtex) 
(package! pydoc) 
(package! rainbow-mode) 
(package! smart-mode-line) 
(package! smex) 
(package! undo-tree)
(package! ggtags)
(package! ibuffer-projectile)
(package! ag)
(package! org-ql)
(package! magithub)
(package! jupyter)
(package! org-mime)
(package! emacsql-sqlite)
(package! smtpmail)
(package! gitter)
