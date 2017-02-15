;;laajenna pakettitietokantaa ja alusta paketit
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
;(add-to-list 'load-path "~/.emacs.d/symon-master/")
;;poistetaan toistaiseksi symon käytöstä
;;(require 'symon) ;minibufferiin system monitor
;;(setq-default symon-monitors '(symon-linux-memory-monitor symon-linux-cpu-monitor symon-windows-network-rx-monitor symon-windows-network-tx-monitor))
(setq use-package-verbose t)
(require 'use-package)
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
  :config
  (progn
    (require 'smartparens-config)
    (sp-use-smartparens-bindings)))
;(setq sml/theme 'respectful)
                                        ;(sml/setup)
(if (eq system-type 'darwin)
    (setq ispell-program-name "/usr/local/Cellar/aspell/0.60.6.1/bin/aspell")
    )
(use-package moe-theme
  :init
  (use-package powerline)
  :config
  (moe-dark)
  (powerline-moe-theme)
  (moe-theme-set-color 'orange))
(use-package web-mode
  :config
  ;;avaa html tiedostot web-moodissa
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    ;;webmode olettaa, että template toolkit on käytössä tavallisilla html tiedostoilla
    (setq web-mode-engines-alist
          '(("template-toolkit"    . "\\.html\\'"))
          )))
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org))
(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transaction 1)
  :config
  :mode "\\.dat\\'")
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(use-package company
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'python-mode-hook #'company-mode)
  (use-package company-anaconda
    :ensure t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-anaconda)))
  (use-package company-jedi
    :disabled t
    :ensure t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-jedi))))
(use-package pydoc-info
  :ensure t)
(use-package cider
  :init
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode))
(use-package elfeed
  :ensure t
  :bind (("C-x w" . elfeed)))
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode))
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))
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
    (counsel-projectile-on))
  (ivy-mode 1)
  (setq ivy-extra-directories nil)) ;do not show ./ and .//

(use-package restclient)
(use-package deft
  :init
  (progn
    (setq deft-extension "org")
    (setq deft-directory "~/Workdocuments")
    (setq deft-use-filename-as-title t)))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))
(use-package sly
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
  :config
  (progn
    ;;poista joistakin viesteistä ilmoittaminen ERC moodista
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
    (setq erc-server-coding-system '(utf-8 . utf-8))
    ;;liity näihin servereihin ja kanaviin automaattisesti käynnistyksen yhteydessä
    (let ((erc-data-file  "~/.emacs.d/ercinfo.el"))
      (if (file-exists-p erc-data-file)
	  (progn 
	    (load erc-data-file)
	    (erc-load-info))))
                                        ;    (add-hook 'erc-mode-hook 'flyspell-mode) ;laita oikeinkirjoitus päälle irkkikanavilla

      ;; other random services (spelling)
    (use-package erc-services
      :init
      (progn
        (setq ispell-dictionary "en_GB")
        (add-to-list 'erc-modules 'spelling)
        (erc-services-mode 1)
        (erc-spelling-mode 1)))
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
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (use-package org-protocol
    :init
    (setq org-capture-templates
	  '(
	    ("b" "Capture link over org-protocol"
	     entry (file+headline "/Users/toniok/bookmarks.org" "Bookmark inbox")
	     "** %:description\n   [[%:link][%:link]] \n   CREATED: %U\n\n   %i"
	     :immediate-finish 1 :empty-lines 1)
	    ("t" "Capture todo over org-protocol"
	     entry (file+headline "/Users/toniok/agenda.org" "Future tasks")
	     "** TODO %:link \n   CREATED: %U\n   SOURCE: %:description\n\n   %:initial"
	     :immediate-finish 1 :empty-lines 1 :prepend t)
	    ("i" "Capture an idea over org-protocol"
	     entry (file+headline "/Users/toniok/blog.org" "Ideas")
	     "** TODO %:link \n   CREATED: %U\n   SOURCE: %:description\n\n   %:initial"
	     :immediate-finish 1 :empty-lines 1 :prepend t))))
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-clock-idle-time 30)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))
(use-package cperl-mode
  :init
  (progn
    (defalias 'perl-mode 'cperl-mode))
  :config
  (progn
    (add-hook 'cperl-mode-hook '(lambda () (linum-mode 1)))))
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
(add-hook 'js2-mode-hook '(lambda () (linum-mode 1))) ;linum päälle javascript tiedostoissa
(add-hook 'web-mode-hook '(lambda () (linum-mode 1))) ;linum päälle myös web-moodissa
(add-hook 'org-mode-hook '(lambda () (hl-line-mode 1))) ;väritä nykyinen rivi org-moodissa
(add-hook 'c++-mode-hook '(lambda () (linum-mode 1)))
(use-package paredit
  :disabled t
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)
   (ipython . t)
   (dot . t)))

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
					      (setq show-paren-style 'expressions))))
  (progn
    (menu-bar-mode -1) 
    (tool-bar-mode -1) 
    (scroll-bar-mode -1)
    (show-paren-mode 1)
    ;highlight between ()
    (setq show-paren-style 'expression))) 

(setq-default indent-tabs-mode nil) ;emacs käyttää tabulaattorin sijasta välilyöntiä
(setq tab-width 4) ;näytä kaikki sarkaimet 4 välilyön kokoisina
(which-function-mode 1) ;näytä alarivillä missä funktiossa kursori tällä hetkellä on
(setq inhibit-startup-message t) ;poista aloitusscreen
(setq password-cache-expiry nil) ;tramp ei kysy salasanaa koko ajan
(setq make-backup-files nil) ;varmista, että Emacs ei tee varmuuskopiotiedostoja. Pidetään serveri näin siistinä
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("dbb643699e18b5691a8baff34c29d709a3ff9787f09cdae58d3c1bc085b63c25" "32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(markdown-command "/usr/local/bin/pandoc")
 '(org-enforce-todo-dependencies t)
 '(package-selected-packages
   (quote
    (which-key ledger-mode web-mode js2-mode restclient elfeed-org elfeed smex flx exec-path-from-shell magit powerline moe-theme zenburn-theme noctilux-theme use-package solarized-theme smartparens php-mode paredit markdown-mode lua-mode helm groovy-mode deft color-theme-solarized cider)))
 '(send-mail-function (quote mailclient-send-it))
 '(sml/mode-width
   (if
       (eq powerline-default-separator
           (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   nil)))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   nil
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(which-function-mode t))

;;aktivoi uusi javascript moodi
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;yhdistä dired bufferit ibufferissa
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))

;;laita trampin autosave kansion lokaaliksi
(setq tramp-auto-save-directory "~/emacs/tramp-autosave")
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
;(global-set-key (kbd "C-c a") 'org-agenda)
;(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/organizer.org")
(setq org-agenda-files '("~/Workdocuments/"))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))))
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
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono" :height (if (eq system-type 'gnu/linux) 
                                                           130
                                                         (if (eq system-type 'darwin)
                                                             140)) :weight 'normal)

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
(defun setup-terminal-client-colors ()
  ;(if (not window-system)
   ;   (set-background-color "white")
  ; )
  )
;(add-hook 'after-make-frame-functions 'setup-terminal-client-colors)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
