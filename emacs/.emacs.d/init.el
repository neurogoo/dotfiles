;;laajenna pakettitietokantaa ja alusta paketit
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  )
(when (>= emacs-major-version 26)
  (global-display-line-numbers-mode))
(setq use-package-verbose t)
(require 'use-package)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )
(use-package diminish
  :ensure t)
(use-package eldoc
  :diminish eldoc-mode)
(use-package winner
  :diminish winner-mode
  :config
  (winner-mode 1))
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  :config
  (use-package flycheck-plantuml
    :ensure t
    :config
    (flycheck-plantuml-setup)))
(use-package company
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'python-mode-hook #'company-mode)
  (add-hook 'emacs-lisp-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook #'company-mode)
  (add-hook 'haskell-mode-hook #'company-mode)
  (use-package company-anaconda
    :disabled t
    :defer t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-anaconda)))
  (use-package company-ghci
    :ensure t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-ghci)))
  (use-package company-jedi
    :ensure t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-jedi))))
(use-package smartparens
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook 'smartparens-mode)
  (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'inferior-lisp-mode-hook 'smartparens-mode)
  (add-hook 'inferior-lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'scheme-mode-hook 'smartparens-mode)
  (add-hook 'scheme-mode-hook 'smartparens-strict-mode)
  (add-hook 'haskell-mode-hook 'smartparens-mode)
  :config
  (progn
    (require 'smartparens-config)
    (sp-use-smartparens-bindings)))
;(setq sml/theme 'respectful)
                                        ;(sml/setup)
(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4))
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'haskell-style)
  (custom-set-variables '(haskell-stylish-on-save t))
  (setq dante-repl-command-line '("cabal" "new-repl" dante-target "--builddir=dist-newstyle/dante"))
  (add-hook 'dante-mode-hook
            '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                   '(warning . haskell-hlint))))
  :config
  (remove-hook 'xref-backend-functions 'dante--xref-backend)
  (setq tags-case-fold-search nil))

(if (eq system-type 'darwin)
    (setq ispell-program-name "/usr/local/bin/aspell")
    )
(use-package moe-theme
  :ensure t
  :init
  (use-package powerline
    :ensure t)
  :config
  (moe-dark)
  (powerline-moe-theme)
  (if (eq system-type 'darwin)
      (moe-theme-set-color 'purple)
    (moe-theme-set-color 'orange)))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)))
(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transaction 1)
  :config
  :mode "\\.dat\\'")
(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'")
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'smartparens-mode)
  :config
  (use-package racer
    :ensure t
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t))
  (use-package flycheck-rust
    :ensure t
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (setq rust-format-on-save t))
(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package pydoc-info
  :ensure t)
(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode))
(use-package elfeed
  :ensure t
  :init
  (setq elfeed-db-directory "~/.emacs.d/elfeeddb")
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  :bind (("C-x w" . elfeed))
  :config
  (use-package elfeed-org
    :ensure t
    :config
    (elfeed-org)
    ))
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "Z_dependencies"))
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))
;;helm moodi päälle aina
(use-package helm
  ;;:ensure t
  :disabled t
  :bind
  ("M-x" . helm-M-x)
  ("C-c g" . helm-google-suggest)
  ("C-x C-f". helm-find-files)
  :init
  (progn
    (use-package helm-config)
    (helm-mode 1)
    (setq helm-M-x-fuzzy-match t)))
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("M-x" . counsel-M-x))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style nil)
  (use-package smex :ensure t)
  (use-package counsel :ensure t)
  (use-package flx
    :ensure t
    :config
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
  :config
  (use-package counsel-projectile
    :ensure t
    :config
    (setq counsel-projectile-mode t))
  (ivy-mode 1)
  (setq ivy-extra-directories nil)) ;do not show ./ and .//

(use-package restclient
  :disabled)
(use-package deft
  :ensure t
  :init
  (progn
    (setq deft-extension "org")
    (setq deft-directory "~/Workdocuments")
    (setq deft-use-filename-as-title t)))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))
(use-package diff-hl
  :ensure t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (custom-set-faces
  '(diff-hl-change ((t (:background "#3a81c3"))))
  '(diff-hl-insert ((t (:background "#7ccd7c"))))
  '(diff-hl-delete ((t (:background "#ee6363")))))
  (global-diff-hl-mode 1))
(use-package sly
  :disabled
  :init
  (progn
    (add-hook 'lisp-mode-hook 'sly-editing-mode)
    (add-hook 'sly-mode-hook 'sly-company-mode)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'sly-company)))
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")))
;;varmistaa, että $PATH luetaan shellistä
(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (progn
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))))
;;(use-package flyspell
;;  :config
;;  (progn
;;    (setq ispell-dictionary "en_GB"))) ;laita sanakirja automaattisesti käyttämään brittienglantia
(use-package erc
  :if (or (daemonp) window-system)
  :defer t
  :config
  (setq erc-track-minor-mode nil)
  (progn
    ;;poista joistakin viesteistä ilmoittaminen ERC moodista
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
    (setq erc-server-coding-system '(utf-8 . utf-8))
    (setq erc-join-buffer 'bury)
    ;; other random services (spelling)
    (use-package erc-services
      :config
      (progn
        (setq ispell-dictionary "en_GB")
        (add-to-list 'erc-modules 'spelling)
        (erc-services-mode 1)
        (erc-spelling-mode 1)))
    ;;liity näihin servereihin ja kanaviin automaattisesti käynnistyksen yhteydessä
    (let ((erc-data-file  "~/.emacs.d/ercinfo2.el"))
      (if (file-exists-p erc-data-file)
	  (progn
	    (load erc-data-file)
	    (erc-load-info))))
                                        ;    (add-hook 'erc-mode-hook 'flyspell-mode) ;laita oikeinkirjoitus päälle irkkikanavilla
    (erc-update-modules)
    ))
;;näytä emacsin undo-toiminto visuaalisempana
;(use-package undo-tree
  ;;:defer t
  ;;:ensure t
;  (progn
;    (global-undo-tree-mode)
;    (setq undo-tree-visualizer-timestamps t)
                                        ;    (setq undo-tree-visualizer-diff t)))
(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))
(use-package nose
  :ensure t)
(use-package racket-mode
  :ensure t
  :defer t)
(use-package geiser
  :ensure t
  :init
  (add-hook 'geiser-repl-mode-hook 'smartparens-strict-mode)
                                        ;(add-hook 'geiser-mode-hook 'eldoc-mode)
  )
(use-package yasnippet
  :ensure t
  :diminish yassnippet
  :config
  (yas-global-mode 1))
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (use-package org-protocol
    :init
    (setq org-capture-templates
	  '(
            ("t" "Todo" entry (file+headline "~/notes.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
            ("n" "note" entry (file+headline "~/notes.org" "Notes")
             "* %? :NOTE:\n%U\n%a\n")
            ("L" "Capture link over org-protocol"
	     entry (file+headline "/Users/toku/bookmarks.org" "Bookmark inbox")
	     "** %:description\n   [[%:link][%:link]] \n   CREATED: %U\n\n   %i"
	     :immediate-finish 1 :empty-lines 1)
	    ("T" "Capture todo over org-protocol"
	     entry (file+headline "/Users/toku/agenda.org" "Future tasks")
	     "** TODO %:link \n   CREATED: %U\n   SOURCE: %:description\n\n   %:initial"
	     :immediate-finish 1 :empty-lines 1 :prepend t)
	    ("p" "Capture an idea over org-protocol"
	     entry (file+headline "/Users/toku/notes.org" "Ideas")
	     "** TODO %:link \n   CREATED: %U\n   SOURCE: %:description\n\n   %:initial"
	     :immediate-finish 1 :empty-lines 1 :prepend t)))
    (setq org-enforce-todo-dependencies t))
  (use-package org-present
    :ensure t)
  (use-package ox-pandoc
    :disabled
    :ensure t
    :config
    (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex"))))
  (use-package ox-reveal
    :disabled
    :config
    (setq org-reveal-root "file:///Users/toniok/Downloads/reveal.js-3.5.0/"))
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-clock-idle-time 30)
  (setq org-default-notes-file "~/notes.org")
  (setq org-agenda-files '("~/Workdocuments/"))
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (add-hook 'org-mode-hook '(lambda () (hl-line-mode 1)))) ;väritä nykyinen rivi org-moodissa
(use-package cperl-mode
  :init
  (progn
    (defalias 'perl-mode 'cperl-mode)))
(use-package google-translate
  ;;:ensure t
  :bind
  ("\C-ct" . google-translate-smooth-translate)
  :config
  (progn
    (use-package google-translate-smooth-ui)
    (setq google-translate-output-destination nil)
    (setq google-translate-pop-up-buffer-set-focus t)
    (setq google-translate-translation-directions-alist '(("fi"."en")("en"."fi")))))
(use-package ob-ipython
  :ensure t)
(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook #'flycheck-mode))
(use-package js2-mode
  :ensure t)
(use-package paredit
  :disabled t
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(use-package tide
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))
(use-package meghanada
  :ensure t
  :init
  ;; Don't auto-start
  (setq meghanada-auto-start nil)
  (add-hook 'java-mode-hook #'meghanada-mode)
  (add-hook 'java-mode-hook 'flycheck-mode))
(use-package dumb-jump
  :ensure t)

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
  (plantuml . t)
  (ipython . t)
  (dot . t)
  (shell . t))
 )

(setq org-plantuml-jar-path
      (expand-file-name "~/plantuml.jar"))

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;;avaa tsv tiedostot csv-moodissa
(add-to-list 'auto-mode-alist '("\\.tsv\\'" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

(if (daemonp)
    (progn
      (add-hook 'after-make-frame-functions (lambda (frame)
					      (when (display-graphic-p frame)
						(menu-bar-mode -1)
						(tool-bar-mode -1)
						(scroll-bar-mode -1))
					      (show-paren-mode 1)
					      (setq show-paren-style 'expressions)
                                              (exec-path-from-shell-initialize))))
  (progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (show-paren-mode 1)
    ;highlight between ()
    (setq show-paren-style 'expression)
    (exec-path-from-shell-initialize)))

(setq-default indent-tabs-mode nil) ;emacs käyttää tabulaattorin sijasta välilyöntiä
(setq tab-width 4) ;näytä kaikki sarkaimet 4 välilyön kokoisina
(which-function-mode 1) ;näytä alarivillä missä funktiossa kursori tällä hetkellä on
(setq inhibit-startup-message t) ;poista aloitusscreen
(setq password-cache-expiry nil) ;tramp ei kysy salasanaa koko ajan
(setq make-backup-files nil) ;varmista, että Emacs ei tee varmuuskopiotiedostoja. Pidetään serveri näin siistinä

;;aktivoi uusi javascript moodi
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;yhdistä dired bufferit ibufferissa
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))

;;laita trampin autosave kansion lokaaliksi
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
(setq tramp-default-method "sshx")

;;poista trampin autosave
;;(defun tramp-set-auto-save ()
;;  (auto-save-mode -1))

;;Funktio pätkä joka poistaa tietyt moodit tiedostosta, jos sen koko on suuri
;;(defun conditional-disable-modes ()
;;  (when (> (buffer-size) 2000000)
;;    (flycheck-mode -1)))
;;
;;(add-hook 'c-mode-hook 'conditional-disable-modes)
;;(add-hook 'c++-mode-hook 'conditional-disable-modes)

;;automaattisesti mahduta tekstirivit näkyvään tilaan
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq TeX-PDF-mode t) ;huolehdi, että latex käännetään aina pdflatexilla

;näytä vastinsulje minibufferissa
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
        "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
	(interactive)
	(let* ((cb (char-before (point)))
	       (matching-text (and cb
				   (char-equal (char-syntax cb) ?\) )
				   (blink-matching-open))))
	  (when matching-text (message matching-text))))
;(load "~/.emacs.d/.secrets") ;ei yleisesti jaettavat säädöt

;(defun on-frame-open (frame)
;  (if (not (display-graphic-p frame))
;      (set-face-background 'default "unspecified-bg" frame)))
;(on-frame-open (selected-frame))
;(add-hook 'after-make-frame-functions 'on-frame-open)
;(defun on-after-init ()
;  (unless (display-graphic-p (selected-frame))
;    (set-face-background 'default "unspecified-bg" (selected-frame))))

;(add-hook 'window-setup-hook 'on-after-init)
;(defun on-after-init ()
;  (unless (display-graphic-p (selected-frame))
;    (set-face-background 'default "unspecified-bg" (selected-frame))))
;
                                        ;(add-hook 'window-setup-hook 'on-after-init)

(cond
 ((find-font (font-spec :name "Iosevka")) (set-face-attribute 'default nil
                    :family "Iosevka" :height (if (eq system-type 'gnu/linux)
                                                           130
                                                         (if (eq system-type 'darwin)
                                                             145)) :weight 'normal))
 ((set-face-attribute 'default nil
                    :family "DejaVu Sans Mono" :height (if (eq system-type 'gnu/linux)
                                                           130
                                                         (if (eq system-type 'darwin)
                                                             140)) :weight 'normal)))

(defun my-web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(defun sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))

    t))
(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

;;funktio tiedoston polun saamiseksi helposti
(define-key global-map (kbd "\C-c w")
  (defun show-file-name ()
    "Show the full path file name in the minibuffer and add it to kill ring"
    (interactive)
    (message (buffer-file-name))
    (kill-new (buffer-file-name))))
;(desktop-save-mode 1) ;lataa aikaisemmat tiedostot

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
`persistent-scratch-file-name'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max) "~/.emacs-persistent-scratch")))

(defun load-persistent-scratch ()
  "Load the contents of `persistent-scratch-file-name' into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p "~/.emacs-persistent-scratch")
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert-file-contents "~/.emacs-persistent-scratch"))))

(push #'load-persistent-scratch after-init-hook)
(push #'save-persistent-scratch kill-emacs-hook)

(if (not (boundp 'save-persistent-scratch-timer))
    (setq save-persistent-scratch-timer
          (run-with-idle-timer 300 t 'save-persistent-scratch)))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun mu-xml-format ()
  "Format an XML buffer with `xmllint'."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "xmllint -format -"
                           (current-buffer) t
                           "*Xmllint Error Buffer*" t))
(autoload 'notmuch "notmuch" "Notmuch mail" t)
;(require 'notmuch)
(defun notmuch-get-date (date)
  "Convert a date for notmuch processing."
  (substring (shell-command-to-string (concat "date --date=\"" date "\" +%s")) 0 -1))
(defun notmuch-today ()
  "Show today's mail."
  (interactive)
  (notmuch-search
   (concat
    (notmuch-get-date "today 0") ".." (notmuch-get-date "now"))))
(defun my-run-hasktags--sentinel (process status)
  (message "Hasktags exited with status: %s" status))
(defun my-run-hasktags ()
  "Generate new hasktags TAGS in ~/hmr folder."
  (interactive)
  (let ((default-directory "/Users/toku/hmr/")
        (temp-buffer-name "*my-hasktags-output*"))
    (set-process-sentinel
     ;;(start-process "Hasktags" nil "hasktags" "-e" "-x" ".")
     (start-process "Hasktags" nil "hasktags" "-e" "-x" "arcola" "avar" "avatar-app" "avatar-client" "aws-lambda-kleisli" "checklist-app" "contacts-api" "deprecated" "dist-newstyle" "dist" "docs.futurice.com" "dynmap-cache" "email-proxy-client" "email-proxy" "env-config" "flowdock-cli" "flowdock-grep" "flowdock-rest" "fum-api" "fum-carbon-app" "fum-client" "fum-types" "futurice-constants" "futurice-foundation" "futurice-github" "futurice-integrations" "futurice-lambda" "futurice-logo" "futurice-metrics" "futurice-postgres" "futurice-prelude" "futurice-pure-trans" "futurice-reports" "futurice-servant" "futurice-tribes" "github-proxy" "github-sync" "haxl-fxtra" "hc-app" "hours-api" "lambdacss" "log-cloudwatch" "mega-repo-tool" "monad-http" "optparse-sop" "periocron" "personio-client" "personio-proxy" "planmill-client" "planmill-proxy" "planmill-sync" "postgresql-simple-url" "proxy-app" "proxy-mgmt-app" "reports-app" "scripts" "servant-Chart" "servant-algebraic-graphs" "servant-binary-tagged" "servant-dashdo" "smileys-app" "sms-proxy" "tdigest" "theme-app" "vendor")
     'my-run-hasktags--sentinel)))
(set-face-attribute 'line-number nil :foreground "#4C566A")
(set-face-attribute 'line-number-current-line nil :foreground "goldenrod")
(use-package linum-highlight-current-line-number
  :disabled t
  :load-path "linum-highlight-current-line-number"
  :config
  (set-face-foreground 'linum "#4C566A")
  (set-face-background 'linum nil)
  (setq linum-format 'linum-highlight-current-line-number))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (tide company-jedi git-gutter diminish which-key use-package treemacs smex smartparens rainbow-delimiters racket-mode racer pydoc-info powerline ox-pandoc org-present ob-ipython nose moe-theme meghanada markdown-mode magit ledger-mode json-mode js2-mode geiser flycheck-rust flycheck-plantuml flx exec-path-from-shell elfeed-org dumb-jump deft dante counsel-projectile company-ghci company-anaconda cider cargo))))
