;;laajenna pakettitietokantaa ja alusta paketit
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  )
(when (>= emacs-major-version 26)
  (global-display-line-numbers-mode))

(defun display-buffer-from-rg-p (_buffer-name _action)
  (unless current-prefix-arg
    (with-current-buffer (window-buffer)
      (derived-mode-p 'rg-mode))))

(defun display-buffer-from-clj-refactor (_buffer-name _action)
  (unless current-prefix-arg
    (with-current-buffer (window-buffer)
      (equal (buffer-name) "*cljr-find-usages*"))))

(push '(display-buffer-from-rg-p display-buffer-use-some-window) display-buffer-alist)
(push '(display-buffer-from-clj-refactor display-buffer-use-some-window) display-buffer-alist)

;;From Emacs docs, function that can be used with defadvice to trafe function call
(defun his-tracing-function (orig-fun &rest args)
  (message "display-buffer called with args %S" args)
  (let ((res (apply orig-fun args)))
    (message "display-buffer returned %S" res)
    res))
;(advice-add 'display-buffer :around #'his-tracing-function)
;(advice-remove 'display-buffer #'his-tracing-function)

(setq use-package-verbose t)

;;Set build-in calendar week day starting with Monday
(setq calendar-week-start-day 1)
(require 'use-package)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosaves" t)))
(use-package lua-mode
  :ensure t)
(use-package dirvish
  :ensure t
  :config
  (dirvish-override-dired-mode))
(use-package rg
  :ensure t
  :config
  (rg-enable-menu)
  ;next-error-follow-minor-mode
  )
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-dispatch-always t)
  (ace-window-display-mode t))
(use-package gnuplot
  :ensure t)
(use-package casual-calc
  :ensure t
  :bind (:map
         calc-mode-map
         ("C-o" . casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-o" . casual-calc-tmenu))
  :after (calc))
(use-package zoom
  :ensure t
  :config
  (custom-set-variables '(zoom-size '(0.618 . 0.618))))
(use-package paradox
  :ensure t
  :defer t
  :custom
  (paradox-execute-asynchronously t)
  :config
  (paradox-enable))
(use-package lean-mode
  :ensure t)
(use-package company-lean
  :ensure t
  :after (lean-mode company))
(use-package lsp-mode
  :init
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 100000000)
  :ensure t
  :commands lsp
  :hook ((elm-mode . lsp)
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp)
         ;(rust-mode . lsp)
         )
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (setq lsp-headerline-breadcrumb-enable nil))
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))
(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4))
(use-package lsp-haskell
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'haskell-style)
  :after (lsp-mode))
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands (lsp-ui-mode)
  :hook ((lsp-mode . lsp-ui-mode))
  :config
  (setq lsp-ui-sideline-enable t))
(use-package lsp-ivy
  :ensure t
  :after (lsp-mode)
  :commands lsp-ivy-workspace-symbol)
(use-package diminish
  :ensure t)
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :ensure t
  :after (tree-sitter))
(use-package popwin
  :ensure t)
(use-package eldoc
  :diminish eldoc-mode
  :config
  (add-hook 'ielm-mode-hook 'eldoc-mode))
(use-package winner
  :diminish winner-mode
  :config
  (winner-mode 1))
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package flycheck-plantuml
  :ensure t
  :after (flycheck)
  :config
  (flycheck-plantuml-setup))
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'python-mode-hook #'company-mode)
  (add-hook 'emacs-lisp-mode-hook #'company-mode)
  ;(add-hook 'rust-mode-hook #'company-mode)
  (add-hook 'haskell-mode-hook #'company-mode)
  (use-package company-anaconda
    :disabled t
    :defer t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-anaconda))))
(use-package company-ghci
  :ensure t
  :after (company)
  :init
  (add-to-list 'company-backends 'company-ghci))
(use-package company-lua
  :ensure t
  :after (company)
  :init
  (setq company-lua-executable "/usr/local/bin/lua")
  (add-to-list 'company-backends 'company-lua)
  :config
  (defun lua-funcname-at-point ()
    "Get current Name { '.' Name } sequence."
    (when (or (lua--funcname-char-p (char-before))
              (lua--funcname-char-p (char-after)))
      (save-excursion
        (save-match-data
          (re-search-backward "\\`\\|[^A-Za-z_.]")
          ;; NOTE: `point' will be either at the start of the buffer or on a
          ;; non-symbol character.
          (re-search-forward "\\([A-Za-z_]+\\(?:\\.[A-Za-z_]*\\)*\\)")
          (match-string-no-properties 1))))))
(use-package company-jedi
  :disabled t
  :ensure t
  :after (company)
  :init
  (add-to-list 'company-backends 'company-jedi))
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
  (add-hook 'elm-mode-hook 'smartparens-mode)
  (add-hook 'ielm-mode-hook 'smartparens-mode)
  (add-hook 'ielm-mode-hook 'smartparens-strict-mode)
  :config
  (progn
    (require 'smartparens-config)
    (sp-use-smartparens-bindings)))
(use-package reformatter
  :ensure t
  :config
  (reformatter-define cabal-format
    :program "cabal-fmt")
  (reformatter-define edn-format
    :program "bb"
    :args '("-e" "(-> *in* clojure.edn/read clojure.pprint/pprint)" "-i"))
  (reformatter-define json-to-edn-format
    :program "bb"
    :args '("-e" "(-> *in* slurp (cheshire.core/parse-string true) clojure.pprint/pprint)" "-i")))

(if (eq system-type 'darwin)
    (setq ispell-program-name "/opt/homebrew/bin/aspell"))
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
(use-package nerd-icons
  :ensure t
  :custom
  (setq nerd-icons-font-family "Iosevka Nerd Font"))
(use-package nerd-icons-ivy-rich
  :ensure t
  :init
  (nerd-icons-ivy-rich-mode 1))
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-env-version t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project))
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
  :mode "\\.plantuml\\'"
  :config
  (setq plantuml-jar-path
      (expand-file-name "~/Downloads/plantuml.jar")))
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))
(use-package rustic
  :ensure t
  :after '(rust-mode)
  :mode "\\.rs\\'"
  ;; :bind (:map rustic-mode-map
  ;;             ("M-j" . lsp-ui-imenu)
  ;;             ("M-?" . lsp-find-references)
  ;;             ("C-c C-c l" . flycheck-list-errors)
  ;;             ("C-c C-c a" . lsp-execute-code-action)
  ;;             ("C-c C-c r" . lsp-rename)
  ;;             ("C-c C-c q" . lsp-workspace-restart)
  ;;             ("C-c C-c Q" . lsp-workspace-shutdown)
  ;;             ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save nil)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'smartparens-mode)
  :config
  (use-package racer
    :disabled t
    :ensure t
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t))
  (use-package flycheck-rust
    :ensure t
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (setq rust-format-on-save t))
(use-package cargo
  :disabled t
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package pydoc-info
  :disabled t
  :ensure t)
(use-package clj-refactor
  :ensure t
  :defer t)
;;For cider log view
(use-package logview
  :ensure t)
(use-package cider
  :ensure t
  :defer t
  :config
  ;(setq cider-cljs-lein-repl
  ;    "(do (require 'figwheel-sidecar.repl-api)
  ;         (figwheel-sidecar.repl-api/start-figwheel!)
  ;         (figwheel-sidecar.repl-api/cljs-repl))")
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (setq cider-ns-code-reload-tool 'clj-reload)
  (setq cider-redirect-server-output-to-repl nil))
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "Z_dependencies")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))
;;helm moodi päälle aina
(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :init
  (setq elm-format-on-save t))
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
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style nil)
  (use-package smex :ensure t)
  (use-package counsel :ensure t)
  (use-package flx
    :ensure t
    :config
    (setq ivy-re-builders-alist '((t . ivy--regex-plus))))
  :config
  (use-package counsel-projectile
    :ensure t
    :config
    (setq counsel-projectile-mode t))
  (ivy-mode 1)
  (setq ivy-extra-directories nil)) ;do not show ./ and .//
(use-package ivy-posframe
  :ensure t
  :after ivy
  :init
;;  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  :config
  (ivy-posframe-mode 1))
(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich--display-transformers-list
        '(counsel-find-file
          (:columns
           ((ivy-read-file-transformer)
            (ivy-rich-counsel-find-file-truename
             (:face font-lock-doc-face))))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer
             (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate
             (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face))))
          package-install
          (:columns
           ((ivy-rich-candidate
             (:width 30))
            (ivy-rich-package-version
             (:width 16 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary
             (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary
             (:face font-lock-doc-face))))))
  (ivy-rich-mode 1)
  )
(use-package deft
  :ensure t
  :init
  (progn
    (setq deft-extension "org")
    (setq deft-directory "~/Workdocuments")
    (setq deft-use-filename-as-title t)))
(use-package magit
  :ensure t
  :config
  (setq magit-repository-directories `(("~/dias" . 1)))
  :bind
  (("C-x g" . magit-status)))
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))
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
  :disabled t
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
(use-package verb
  :ensure t)
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (require 'ox-md)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-hide-leading-stars t)
  (use-package org-protocol
    :init
    (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                   (file+headline "~/Workdocuments/inbox.org" "Tasks")
                                   "* TODO %i%?")
                                  ("T" "Tickler" entry
                                   (file+headline "~/Workdocuments/tickler.org" "Tickler")
                                   "* %i%? \n %U")))
    (setq org-enforce-todo-dependencies t))
  (use-package org-present
    :ensure t)
  (use-package ox-pandoc
    :disabled
    :ensure t
    :config
    (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex"))))
  (use-package ox-reveal
    :ensure t
    :config
    (use-package htmlize
      :ensure t)
    (setq org-reveal-root "file:///Users/toku/Downloads/reveal.js-3.7.0/"))
  (use-package org-tree-slide
    :ensure t)
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-clock-idle-time 30)
  (setq org-default-notes-file "~/notes.org")
  (when (and (file-exists-p "~/Workdocuments/inbox.org")
             (file-exists-p "~/Workdocuments/gtd.org")
             (file-exists-p "~/Workdocuments/tickler.org"))
    (progn
;;      (setq org-agenda-files '("~/Workdocuments/inbox.org" "~/Workdocuments/gtd.org" "~/Workdocuments/tickler.org"))
      (setq org-agenda-files '("~/Workdocuments/inbox.org"))
      (setq org-refile-targets '(("~/Workdocuments/gtd.org" :maxlevel . 3)
                                 ("~/Workdocuments/someday.org" :level . 1)
                                 ("~/Workdocuments/tickler.org" :maxlevel . 2)))))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (org-agenda-list 1)
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
(use-package json-mode
  :defer t
  :ensure t
  :config
  (add-hook 'json-mode-hook #'flycheck-mode))
(use-package js2-mode
  :defer t
  :ensure t)
(use-package vue-mode
  :defer t
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
  ;; use Prettier instead
;  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))
(use-package prettier
  :ensure t)
(use-package web-mode
  :ensure t
  :after (tide)
  :mode "\\.tsx\\'"
  :config
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;; (defun my-web-mode-hook ()
;;   (setq web-mode-enable-auto-pairing nil))

;; (add-hook 'web-mode-hook  'my-web-mode-hook)

(use-package ob-typescript
  :ensure t)
(use-package idris-mode
  :mode "\\.idr\\'"
  :ensure t)
(use-package dumb-jump
  :ensure t)
(use-package flycheck-clj-kondo
  :ensure t)
(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (setq cljr-warn-on-eval nil)
  (yas-minor-mode 1))
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
  (plantuml . t)
  (dot . t) ;graphviz
  (shell . t)
  (typescript . t)
  (sql . t)
  (python . t)))

(setq org-plantuml-jar-path
      (expand-file-name "~/Downloads/plantuml.jar"))

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;;avaa tsv tiedostot csv-moodissa
(add-to-list 'auto-mode-alist '("\\.tsv\\'" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

(defcustom csv+-quoted-newline "\^@"
  "Replace for newlines in quoted fields."
  :group 'sv
  :type 'string)

(defun csv+-quoted-newlines (&optional b e inv)
  "Replace newlines in quoted fields of region B E by `csv+-quoted-newline'.
B and E default to `point-min' and `point-max', respectively.
If INV is non-nil replace quoted `csv+-quoted-newline' chars by newlines."
  (interactive
   (append (when (region-active-p)
         (list (region-begin)
           (region-end)))
       prefix-arg))
  (unless b (setq b (point-min)))
  (unless e (setq e (point-max)))
  (save-excursion
    (goto-char b)
    (let ((from (if inv csv+-quoted-newline "\n"))
      (to (if inv "\n" csv+-quoted-newline)))
      (while (search-forward from e t)
    (when (nth 3 (save-excursion (syntax-ppss (1- (point)))))
      (replace-match to))))))

(defsubst csv+-quoted-newlines-write-contents ()
  "Inverse operation of `csv+-quoted-newlines' for the full buffer."
  (save-excursion
    (save-restriction
      (widen)
      (let ((file (buffer-file-name))
        (contents (buffer-string)))
    (with-temp-buffer
      (insert contents)
      (csv+-quoted-newlines (point-min) (point-max) t)
      (write-region (point-min) (point-max) file)))))
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  t ;; File contents has been written (see `write-contents-functions').
  )

(defun csv+-setup-quoted-newlines ()
  "Hook function for `csv-mode-hook'.
Transform newlines in quoted fields to `csv+-quoted-newlines'
when reading files and the other way around when writing contents."
  (add-hook 'write-contents-functions #'csv+-quoted-newlines-write-contents t t)
  (let ((modified-p (buffer-modified-p)))
    (csv+-quoted-newlines)
    (set-buffer-modified-p modified-p)))

(add-hook 'csv-mode-hook #'csv+-setup-quoted-newlines)

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
(setq js-indent-level 2)

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
;(add-hook 'before-save-hook 'delete-trailing-whitespace)
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
				   (blink-matching-open))))))
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
 ((find-font (font-spec :name "Iosevka Nerd Font"))
  (set-face-attribute 'default nil
                      :family "Iosevka Nerd Font"
                      :height (if (eq system-type 'gnu/linux)
                                  130
                                (if (eq system-type 'darwin)
                                    180))
                      :weight 'normal))
 ((set-face-attribute 'default nil
                    :family "DejaVu Sans Mono" :height (if (eq system-type 'gnu/linux)
                                                           130
                                                         (if (eq system-type 'darwin)
                                                             140)) :weight 'normal)))

;;funktio tiedoston polun saamiseksi helposti
(define-key global-map (kbd "\C-c w")
  (defun show-file-name ()
    "Show the full path file name in the minibuffer and add it to kill ring"
    (interactive)
    (message (buffer-file-name))
    (kill-new (buffer-file-name))))
;(desktop-save-mode 1) ;lataa aikaisemmat tiedostot

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
(defmacro make-hasktags-string (file-list)
  `(append '(start-Hasktags "process" nil "hasktags" "-e" "-x") ,file-list))
(defun my-run-hasktags ()
  "Generate new hasktags TAGS in ~/hmr folder."
  (interactive)
  (let ((default-directory "/Users/toku/hmr/")
        (temp-buffer-name "*my-hasktags-output*")
        (hmr-file-list (seq-remove
                        (lambda (str) (string-match-p "TAGS\\|Z_dependencies\\|vendor" str))
                        (seq-filter
                         (lambda (str) (string-match-p "^[a-z]" str))
                         (directory-files "/Users/toku/hmr/")))))
    (set-process-sentinel
     (make-hasktags-string hmr-file-list)
     'my-run-hasktags--sentinel)))
(defun my-haskell-pointfree (beg end)
  (interactive "r")
  (let ((str (buffer-substring beg end)))
    (delete-region beg end)
    (call-process "pointfree" nil t nil str)
    (delete-char -1)))
(set-face-attribute 'line-number nil :foreground "#8a8a8a" :background nil)
(set-face-attribute 'line-number-current-line nil :foreground "goldenrod")
(use-package linum-highlight-current-line-number
  :disabled t
  :load-path "linum-highlight-current-line-number"
  :config
  (set-face-foreground 'linum "#4C566A")
  (set-face-background 'linum nil)
  (setq linum-format 'linum-highlight-current-line-number))

(use-package gptel
  :ensure t)
;;Should be kept as a last thing because of initialization stuff
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))
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
 '(cider-enrich-classpath t)
 '(company-lua-interpreter 'love)
 '(counsel-rg-base-command
   "rg -S -M 160 --no-heading --line-number --color never %s .")
 '(dante-methods-alist
   '((styx "styx.yaml"
           ("styx" "repl" dante-target))
     (nix dante-cabal-nix
          ("nix-shell" "--pure" "--run"
           (concat "cabal repl "
                   (or dante-target "")
                   " --builddir=dist/dante")))
     (impure-nix dante-cabal-nix
                 ("nix-shell" "--run"
                  (concat "cabal repl "
                          (or dante-target "")
                          " --builddir=dist/dante")))
     (nix-ghci
      #[257 "\300\1\301\302#\207"
            [directory-files t "shell.nix\\|default.nix"]
            5 "\12\12(fn D)"]
      ("nix-shell" "--pure" "--run" "ghci"))
     (mafia "mafia"
            ("mafia" "repl" dante-target))
     (bare-cabal
      #[257 "\300\1\301\302#\207"
            [directory-files t ".cabal$"]
            5 "\12\12(fn D)"]
      ("cabal" "new-repl" dante-target "--builddir=dist/dante"))
     (bare-ghci
      #[257 "\300\207"
            [t]
            2 "\12\12(fn _)"]
      ("ghci"))))
 '(dictionary-server "dict.org")
 '(haskell-stylish-on-save t)
 '(idris-interpreter-path "idris2")
 '(lsp-enable-snippet nil)
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]dist$" "[/\\\\]dist-newstyle$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.vscode$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]\\vendor$" "[/\\\\]\\dist-newstyle$"))
 '(lsp-haskell-formatting-provider "stylish-haskell")
 '(lua-default-application "love")
 '(magit-repolist-columns
   '(("Name" 25 magit-repolist-column-ident nil)
     ("Branch" 25 magit-repolist-column-branch nil)
     ("Version" 25 magit-repolist-column-version
      ((:sort magit-repolist-version<)))
     ("B<U" 3 magit-repolist-column-unpulled-from-upstream
      ((:right-align t)
       (:sort <)))
     ("B>U" 3 magit-repolist-column-unpushed-to-upstream
      ((:right-align t)
       (:sort <)))
     ("Path" 99 magit-repolist-column-path nil)))
 '(org-agenda-files '("~/Workdocuments/timetracking.org"))
 '(org-roam-directory "/Users/toni.okuogume@futurice.com/org-roam-docs/")
 '(package-selected-packages
   '(gptel magit-todos casual-calc prodigy zoom gnuplot logview casual verb uiua-ts-mode 0blayout compat hl-todo rg dirvish company-lua lua-mode yaml-mode rustic clj-refactor kibit-helper envrc flycheck-clj-kondo tree-sitter-langs tree-sitter web-mode prettier company-lean lean-mode lean dockerfile-mode lsp-python-ms org-roam vue-mode csv-mode esup paradox all-the-icons-ivy elm-mode org-tree-slide reformatter doom-themes doom-modeline all-the-icons ws-butler eyebrowse htmlize ox-reveal popwin purescript-mode psc-ide idris-mode tide company-jedi git-gutter diminish which-key use-package treemacs smex smartparens rainbow-delimiters racket-mode racer pydoc-info powerline ox-pandoc org-present ob-ipython nose moe-theme meghanada markdown-mode magit ledger-mode json-mode js2-mode geiser flycheck-rust flycheck-plantuml flx exec-path-from-shell elfeed-org dumb-jump deft dante counsel-projectile company-ghci company-anaconda cider cargo))
 '(paradox-execute-asynchronously t t)
 '(paradox-github-token t)
 '(projectile-globally-ignored-directories
   '("Z_dependencies" ".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".clangd" "vendor"))
 '(zoom-size '(0.618 . 0.618)))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
