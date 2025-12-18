;; Lightweight emacs set-up
;; Upgraded completion and search
;; LSP for python and C using uv/jedi and ccls

(setq-default
 inhibit-startup-screen t               ; Disable start-up screen
 inhibit-startup-message t              ; Disable startup message
 inhibit-startup-echo-area-message t    ; Disable initial echo message
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 initial-buffer-choice t                ; Open *scratch* buffer at init
 ring-bell-function 'ignore             ; Stop bell sounds
)

(scroll-bar-mode -1) ; Disable scroll bar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips
(menu-bar-mode -1) ; Disable the menu bar
(add-to-list 'default-frame-alist '(undecorated . t)) ; Remove top bar
(winner-mode t) ; Allow to undo window configurations
(savehist-mode t) ; Save minibuffer histories
(recentf-mode t) ; Keeps track of recently visited files
(which-function-mode 1) ; Display in mode line the current function
(windmove-default-keybindings) ; Move buffers with S-<arrow key>

;; Save ~ files and other backups all together
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Initialize package sources
(require 'package)

;; Set archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
 			 ("org" . "https://orgmode.org/elpa/")
 			 ("elpa" . "https://elpa.gnu.org/packages/")))

;; Allows using older versions of packages for greater compatibility
(add-to-list 'package-archives
	     (cons "gnu-devel" "https://elpa.gnu.org/devel/") t)

;; Initialize use-package on non-Linux platform
(unless (package-installed-p 'use-package) ; use-package not installed by default
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; To modify modeline display of minor modes
(use-package delight
  :ensure t)

;; Themes
(use-package ef-themes
  :ensure t
  :config
  ;; Use ef-themes-toggle to cycle through these
  (setq ef-themes-to-toggle '(ef-day ef-dream)))
(use-package doric-themes
  :ensure t
  :config
  (doric-themes-select 'doric-obsidian))

;; Make the UI slick
(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-subtle-frame-lines
      '( :mode-line-active spacious-padding-line-active
         :mode-line-inactive spacious-padding-line-inactive
         :header-line-active spacious-padding-line-active
         :header-line-inactive spacious-padding-line-inactive)))
(spacious-padding-mode 1)

;; Custom mode line
(setopt mode-line-format
        '("%*"
          vc-mode
	  mode-line-front-space
          mode-line-buffer-identification
	  (:eval (which-function))
          mode-line-format-right-align
	  mode-line-modes
          project-mode-line
	  project-mode-line-format
	  " "
	  buffer-file-name
	  ))

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		term-mode-hook
		vterm-mode-hook
		magit-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Nice vertical menus
(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t))

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

;; Necessary to use both packages
(use-package embark-consult
  :ensure t)

;; Act on selection
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
  ("M-z" . consult-ripgrep)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  )

;; Autocompletion
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

;; Pretty icons
(use-package nerd-icons)

;; Pretty icons when using completion
(use-package nerd-icons-corfu
  :ensure t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Show pretty icons in dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package eldoc
  :delight eldoc-mode
  :ensure t)

;; Show potential commands
(use-package which-key
  :delight which-key-mode
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

;; Treesit install paths
(when (boundp 'treesit-extra-load-path)
  (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
  (add-to-list 'treesit-extra-load-path "~/.local/lib/"))
;; Treesit tested versions
(setq treesit-language-source-alist
      '((c "git@github.com:tree-sitter/tree-sitter-c.git" "v0.20.7")
	(cpp "git@github.com:tree-sitter/tree-sitter-cpp.git" "v0.23.2")
	(python "git@github.com:tree-sitter/tree-sitter-python.git" "v0.23.3")))
;; Use treesiter modes
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
	(c-mode . c-ts-mode)
	(cpp-mode . cpp-ts-mode)))

;; So I can use my own indentation
(setq c-syntactic-indentation nil)

;; Language server protocol
;; For uv, see online installation instructions
;; Need to separately install ccls, sudo apt install works
(use-package eglot
  :ensure t
  :hook ((python-ts-mode . eglot-ensure)
	 (c-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
	       '(c-ts-mode . "ccls")
	       '(python-ts-mode . ("uv" "tool" "run" "jedi-language-server")))
  )

(use-package autorevert
  :delight auto-revert-mode
  :ensure t)

(use-package flymake
  :delight flymake-mode
  :ensure t)

(use-package flymake-ruff
  :ensure t
  :hook (python-ts-mode . flymake-ruff-load))

;; For scheme
(use-package geiser-mit
  :ensure t)

;; Set the font everywhere
(set-frame-font "0xProto Nerd Font" nil t)
(let ((faces '(mode-line
               mode-line-buffer-id
               mode-line-emphasis
               mode-line-highlight
               mode-line-inactive
	       default)))
     (mapc
      (lambda (face) (set-face-attribute face nil :height 90))
      faces))

(defun set-font-size (new-size)
  ;; Change font size for a few different modes
  (interactive "nNew font size: ")
  (let ((faces '(mode-line
		 mode-line-buffer-id
		 mode-line-emphasis
		 mode-line-highlight
		 mode-line-inactive
		 default)))
    (mapc
     (lambda (face) (set-face-attribute face nil :height (* new-size 10)))
     faces)))

;; Terminal replacement
(use-package vterm
  :ensure t
  :bind (("C-c v" . vterm)))

(use-package envrc
  :delight envrc-mode
  :hook (after-init . envrc-global-mode))

(use-package vundo
  :ensure t)

(use-package org2blog
  :ensure t
  :bind ("C-c w" . org2blog-user-interface))

(setq org2blog/wp-blog-alist
      '(("Orthogonal Projections"
         :url "https://orthogonal-projections.com/xmlrpc.php"
         :username ""
	 :password "")))

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

;; Command to resize a window to ~60% of the overall size
(defun window-resize-to-66-percent ()
  (interactive)
  (window-resize nil (- (truncate (* 0.66666 (frame-width))) (window-width)) t))

;; ANSI colors when using M-x compile
(use-package fancy-compilation
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

;; Make music!
(use-package tidal
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auto-revert corfu csv-mode delight doric-themes ef-themes eglot
		 embark-consult envrc fancy-compilation flymake-ruff
		 geiser-mit magit marginalia markdown-mode
		 nerd-icons-corfu nerd-icons-dired orderless org2blog
		 spacious-padding tidal vertico vterm vundo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
