;;; package  --- Summary
;; personalised init.el

;;; Commentary:
;; implements some quality of life improvements
;; uses eglot for the language server protocol
;; install vterm (with external installs needed)
;; uses counsel and ivy for better navigation

(setq-default
 inhibit-startup-screen t               ; Disable start-up screen
 inhibit-startup-message t              ; Disable startup message
 inhibit-startup-echo-area-message t    ; Disable initial echo message
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 initial-buffer-choice t)               ; Open *scratch* buffer at init

;; Save ~ files and other backups all together
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(scroll-bar-mode -1) ; Disable scroll bar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips
(menu-bar-mode -1) ; Disable the menu bar

(winner-mode t) ; Allow to undo window configurations

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
 			 ("org" . "https://orgmode.org/elpa/")
 			 ("elpa" . "https://elpa.gnu.org/packages/")))

;; Allows using older versions of packages for greater
;; compatibility
(add-to-list 'package-archives
	     (cons "gnu-devel" "https://elpa.gnu.org/devel/")
 	     t)

;; Initialize use-package on non-Linux platform
(unless (package-installed-p 'use-package) ; use-package not installed by default
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(global-display-line-numbers-mode t) ; Activate display of line number

;; Make it pretty
(use-package ef-themes
  :ensure t
  :config
  ;; Use ef-themes-toggle to cycle through these
  (setq ef-themes-to-toggle '(ef-day ef-dream))
  (ef-themes-select 'ef-dream))

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-subtle-frame-lines
      '( :mode-line-active spacious-padding-line-active
         :mode-line-inactive spacious-padding-line-inactive
         :header-line-active spacious-padding-line-active
         :header-line-inactive spacious-padding-line-inactive)))
(spacious-padding-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		term-mode-hook
		vterm-mode-hook
		magit-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(dolist (mode '(org-mode-hook
		python-ts-mode-hook
		c-ts-mode-hook
		c++-ts-mode-hook))
  (add-hook mode (lambda () (visual-line-mode 1))))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t))

;; Provides search and navigation commands
(use-package consult
  :bind
  ("C-x b" . consult-buffer)
  ("C-x C-b" . consult-buffer) ; I never use the alternative bind
  ("M-g M-g" . consult-goto-line)
  ("M-g g" . consult-goto-line) ; I never use the alternative bind
  ("C-x f" . consult-find) ; I never use the alternative bind
  ("C-s" . consult-line)
  ("C-c s" . isearch-forward)
  ("M-g i" . consult-imenu)
  )

(use-package consult-ag
  :bind
  ("M-z" . consult-ag))

;; Backend completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Suggests keybindings
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
;; only need to install it, embark loads it after consult if found
(use-package embark-consult
  :ensure t)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu globally.
  :init
  (global-corfu-mode))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Cousine Nerd Font Mono")
  )

(use-package nerd-icons-corfu
  :ensure t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Show pretty icons in dired mode too
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Gives more useful completion when you start typing a command
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; git porcelain
;; Main control is C-x g
(use-package magit
  :ensure t
  :config
  (setq magit-save-repository-buffers nil))

(use-package eglot
  :ensure t
  :hook ((python-ts-mode . eglot-ensure)
	 (python-ts-mode . electric-pair-mode)
	 (python-ts-mode . electric-quote-mode)
	 (python-ts-mode . electric-indent-mode)
	 (c-ts-mode . eglot-ensure)
	 (c-ts-mode . (lambda () (setq comment-start "//" comment-end "")))
	 (c-ts-mode . electric-pair-mode)
	 (c-ts-mode . electric-quote-mode)
	 (c-ts-mode . electric-indent-mode)))

;; Need to separately install ccls, sudo apt install works
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '(c-ts-mode . ("ccls"))))

;; For scheme
(use-package geiser-mit
  :ensure t)

;; Download the font if it doesn't exist.
;; Needed for nerd-icons to function
(unless (member "Cousine Nerd Font" (font-family-list))
   (nerd-icons-install-fonts))

;; Set the font everywhere
(set-frame-font "Cousine Nerd Font" nil t)
(let ((faces '(mode-line
               mode-line-buffer-id
               mode-line-emphasis
               mode-line-highlight
               mode-line-inactive
	       default)))
     (mapc
      (lambda (face) (set-face-attribute face nil :height 90))
      faces))

;; Terminal replacement
(use-package vterm
  :ensure t
  :bind (("C-c v" . vterm)))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package org2blog
  :ensure t
  :bind ("C-c w" . org2blog-user-interface))

(setq org2blog/wp-blog-alist
      '(("Orthogonal Projections"
         :url "https://orthogonal-projections.com/xmlrpc.php"
         :username 
	 :password )))

(setq org2blog/wp-use-sourcecode-shortcode nil)

(setq org2blog/wp-image-upload t)

(setq org2blog/wp-use-wp-latex nil)

;; A few scrolling utils
(keymap-global-set "C-S-l" "C-n C-l")
(keymap-global-set "C-M-l" "C-p C-l")
(keymap-global-set "C-S-n" 'forward-list)
(keymap-global-set "C-S-p" 'backward-list)
(keymap-global-set "C-S-f" 'forward-sexp)
(keymap-global-set "C-S-b" 'backward-sexp)
(keymap-global-set "C-S-k" 'kill-sexp)
(keymap-global-set "C-S-SPC" 'mark-sexp)

;; Save minbuffer histories
(savehist-mode 1)

;; Keeps track of recently visited files
(recentf-mode 1)

;; Modeline stolen from Prot
;; (add-to-list 'load-path (locate-user-emacs-file "prot-modeline"))
;; (require 'prot-emacs-modeline)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key vterm vertico treesit-auto spacious-padding org2blog orderless nerd-icons-dired nerd-icons-corfu markdown-mode marginalia magit keycast geiser-mit envrc embark-consult ef-themes doric-themes csv-mode corfu consult-ag ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
