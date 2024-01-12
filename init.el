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

(scroll-bar-mode -1) ; Disable scroll bar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give more breathing room

(set-face-attribute 'default nil :height 80)

(set-frame-parameter nil 'alpha-background 100) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth

(menu-bar-mode -1) ; Disable the menu bar

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-b" . counsel-ibuffer) ; I never use the alternative bind
	 ("C-x C-f" . counsel-find-file)
	 ("C-x f" . counsel-find-file))) ; I never use the alternative bind

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package ef-themes
  :ensure t)
;; Use ef-themes-toggle to cycle through these
(setq ef-themes-to-toggle '(ef-autumn ef-symbiosis ef-maris-dark ef-elea-dark ef-duo-dark ef-dark ef-night))
;; Change this to change the default theme
(load-theme 'ef-symbiosis :no-confirm)

;; This changes the bar at the bottom of the screen
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

;; Doom modeline only works with these and not "all-the-icons" anymore
(use-package nerd-icons
  :ensure t)

;; Make parentheses different colors to easily tell how they close
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Gives more useful completion when you start typing a command
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; Makes the stuff ivy shows more user-friendly, not sure it's useful?
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Makes help interface more contextual
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode))

;; git porcelain (porcelain = make it nice)
;; Main control is C-x g
(use-package magit
  :ensure t
  :config
  (setq magit-save-repository-buffers nil))

(use-package pyenv-mode
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package poetry
  :ensure t
  :bind (("C-c p" . poetry)
	 ("C-c r" . poetry-run)))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 2)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.2)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd"))))

;; For some reason can't add this to use-package above
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Allows pet below to find the shell path properly on macos
;; will this break anything on linux?
(use-package exec-path-from-shell
  :if (memq (window-system) '(mac ns))
  :config (exec-path-from-shell-initialize))

;; Allows eglot to always find your python env when set with pyenv or poetry
;; config stolen from github page
(use-package pet
  ;:ensure-system-package (dasel sqlite3)
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))

              (pet-eglot-setup)
              (eglot-ensure)

              (setq-local lsp-jedi-executable-command
                          (pet-executable-find "jedi-language-server")))))


;; Don't blow out the minibuffer with company
(setq eldoc-echo-area-use-multiline-p nil)

;; Some extra python fluff
(add-hook 'python-mode-hook (lambda () fill-column 120))

;; Download the font if it doesn't exist.
;; Needed for nerd-icons to function
(unless (member "Cousine Nerd Font" (font-family-list))
  (nerd-icons-install-fonts))

(use-package nerd-icons)

;; Show pretty icons in dired mode too
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Set the font everywhere
(set-frame-font "Cousine Nerd Font 12" nil t)
(setq doom-unicode-font (font-spec :family "Cousine Nerd Font" :size 11))

;; Terminal replacement
(use-package vterm
  :ensure t
  :bind (("C-c v" . vterm)))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exec-path-from-shell nerd-icons-dired vterm ef-themes csv-mode company-box company poetry python-mode magit helpful ivy-rich which-key rainbow-delimiters doom-modeline all-the-icons gruvbox-theme counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
