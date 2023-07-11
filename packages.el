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

;; From DistroTube
(package! org-auto-tangle)              ;autotangling README.org to README.el

(package! beacon)

;; Scimax
(package! ox-pandoc)

(package! ox-ipynb :recipe
  (:host github
   :repo "jkitchin/ox-ipynb"
   :files ("*.el")))

(package! org-ref :recipe
  (:host github
   :repo "jkitchin/org-ref"))
(package! pretty-hydra)

(package! hydra)
(package! xref)

(package! org-mac-link)

(package! google-this)

(package! org-download)

(package! org-gtd)

(package! org-pdfview)
(package! org-pdftools)
(package! org-noter-pdftools)

(package! aggressive-indent)
(package! auto-complete) 

(package! esup)
(package! hy-mode)
(package! mustache) 
(package! pydoc)

(package! smart-mode-line) 
(package! smex) 
(package! ggtags)
(package! ag)
(package! org-ql)
(package! magithub)
(package! jupyter)
(package! org-mime)
(package! emacsql-sqlite)
(package! smtpmail)
(package! gitter)

(package! scimax :recipe
  (:local-repo "~/scimax"))

(package! f)
(package! diminish)
(package! lispy)
(package! ibuffer-projectile)           ; To make scimax-notebook work

(package! elfeed-score)
(package! elfeed-goodies)


(package! ess)
(package! ess-R-data-view)
(package! ess-smart-equals)
(package! ess-smart-underscore)
(package! polymode)

(package! cc-mode)

(package! cl-lib)

;; the eval and compile seems especially necessary for native compilation on the newest Emacs.

(package! dash)
(package! s)

(package! hydra)

(package! org-ai)

;; Biblio
(unpin! org-roam bibtex-completion helm-bibtex ivy-bibtex)
(package! org-roam-ui)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; From Ivy module
(package! swiper)
(package! ivy)
(package! helm)
(package! ivy-hydra)
(package! ivy-avy)
(package! counsel)

(package! bibtex-completion)
(package! ivy-bibtex)
(package! helm-bibtex)
(package! ivy-xref)
(package! ivy-yasnippet)
(package! helm-xref)

(package! amx :pin "37f9c7ae55eb0331b27200fb745206fc58ceffc0")
(package! counsel-projectile :pin "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b")
(package! ivy-rich :pin "600b8183ed0be8668dcc548cc2c8cb94b001363b")
(package! wgrep :pin "edf768732a56840db6879706b64c5773c316d619")

(if (modulep! +prescient)
    (package! ivy-prescient :pin "35cf5d36132c3e96db9a9e4d1902dcfa207e7baa")
  (when (modulep! +fuzzy)
    (package! flx :pin "7b44a5abb254bbfbeca7a29336f7f4ebd8aabbf2")))

(when (modulep! +childframe)
  (package! ivy-posframe :pin "533a8e368fcabfd534761a5c685ce713376fa594"))

(package! all-the-icons-ivy :pin "a70cbfa1effe36efc946a823a580cec686d5e88d")

(unpin! citar)
(package! citar)


(package! keycast)
(package! header2)

(package! command-log-mode)
(package! org-tree-slide)
