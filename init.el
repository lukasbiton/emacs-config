;;; package  --- Summary
;; personalised init.el

;;; Commentary:
;; implements some quality of life improvements
;; uses eglot for the language server protocol
;; install vterm (with external installs needed)
;; uses counsel and ivy for better navigation

;;; Code:

;; No more intro messages about the tutorial
(setq inhibit-startup-message t)

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
(set-fringe-mode 10) ; Give more breathing room

(set-frame-parameter nil 'alpha-background 100) ; For current frame, transparency
(add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth

(menu-bar-mode -1) ; Disable the menu bar

(winner-mode t) ; Allow to undo window configurations

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Save emacs set-up on quitting
(desktop-save-mode 1)

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

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platform
(unless (package-installed-p 'use-package) ; use-package not installed by default
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode) ; Allow display of line number
(global-display-line-numbers-mode t) ; Activate display of line number

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		term-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(dolist (mode '(org-mode-hook
		python-mode-hook
		c-mode-hook
		c++-mode-hook))
  (add-hook mode (lambda () (visual-line-mode 1))))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package vertico
  :init (vertico-mode))

;; Provides search and navigation commands
(use-package consult
  :bind
  ("C-x b" . consult-buffer)
  ("C-x C-b" . consult-buffer) ; I never use the alternative bind
  ("M-g M-g" . consult-goto-line)
  ("M-g g" . consult-goto-line) ; I never use the alternative bind
  ("C-x f" . consult-find) ; I never use the alternative bind
  ("C-s" . consult-line))

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
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

;; Make it pretty
(use-package ef-themes
  :ensure t)
;; Use ef-themes-toggle to cycle through these
(setq ef-themes-to-toggle '(ef-autumn ef-symbiosis ef-maris-dark ef-elea-dark ef-duo-dark ef-dark ef-night))
;; Change this to change the default theme
(load-theme 'ef-dark :no-confirm)

;; This changes the bar at the bottom of the screen
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package eldoc-box
  ;; :config
  ;; (defun rex/eldoc-box-scroll-up ()
  ;;   "Scroll up in `eldoc-box--frame'"
  ;;   (interactive)
  ;;   (with-current-buffer eldoc-box--buffer
  ;;     (with-selected-frame eldoc-box--frame
  ;;       (scroll-down 3))))
  ;; (defun rex/eldoc-box-scroll-down ()
  ;;   "Scroll down in `eldoc-box--frame'"
  ;;   (interactive)
  ;;   (with-current-buffer eldoc-box--buffer
  ;;     (with-selected-frame eldoc-box--frame
  ;;       (scroll-up 3))))
  :bind
  ("M-j" . eldoc-box-help-at-point))

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load)
  (eglot-managed-mode . flymake-ruff-load))

;; Doom modeline only works with these and not "all-the-icons" anymore
(use-package nerd-icons
  :ensure t)

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
  :hook ((python-ts-mode . eglot-ensure)))

;; Download the font if it doesn't exist.
;; Needed for nerd-icons to function
(unless (member "Cousine Nerd Font" (font-family-list))
  (nerd-icons-install-fonts))

;; Set the font everywhere
(set-frame-font "Cousine Nerd Font 10" nil t)
(setq doom-unicode-font (font-spec :family "Cousine Nerd Font" :size 10))

;; Terminal replacement
(use-package vterm
  :ensure t
  :bind (("C-c v" . vterm)))

;; To set the environment for each buffer, helps with LSPs and such
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; To switch windows faster
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flymake-ruff which-key vterm vertico treesit-auto rainbow-delimiters pyenv-mode pet orderless nerd-icons-dired markdown-mode marginalia magit flycheck embark-consult eldoc-box eglot ef-themes doom-modeline csv-mode corfu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
