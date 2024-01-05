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

;; Make it so C-x f and C-x C-f both find file
;; Make ESC quit prompts
(global-set-key (kbd "C-x f") 'find-file)

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
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; The theme overrides line numbers scaling with text scale somehow...
;; (use-package gruvbox-theme)
;; (load-theme 'gruvbox-dark-hard t)

(use-package ef-themes
  :ensure t)
;; Use ef-themes-toggle to cycle through these
(setq ef-themes-to-toggle '(ef-autumn ef-symbiosis ef-maris-dark ef-elea-dark ef-duo-dark ef-dark ef-night))
;; Change this to change the default theme
(load-theme 'ef-symbiosis :no-confirm)

;; (load-theme 'modus-vivendi)

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
  :hook (after-init . global-company-mode))
  ;; :custom
  ;; ;; Search other buffers with the same modes for completion instead of
  ;; ;; searching all other buffers.
  ;; (company-dabbrev-other-buffers t)
  ;; (company-dabbrev-code-other-buffers t)
  ;; ;; M-<num> to select an option according to its number.
  ;; (company-show-numbers t)
  ;; ;; Only 2 letters required for completion to activate.
  ;; (company-minimum-prefix-length 3)
  ;; ;; Do not downcase completions by default.
  ;; (company-dabbrev-downcase nil)
  ;; ;; Even if I write something with the wrong case,
  ;; ;; provide the correct casing.
  ;; (company-dabbrev-ignore-case t)
  ;; ;; company completion wait
  ;; (company-idle-delay 0.2)
  ;; ;; No company-mode in shell & eshell
;; (company-global-modes '(not eshell-mode shell-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; (use-package corfu
;;   :init (global-corfu-mode))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("~/Projects/trading-bot/.venv/bin/jedi-language-server")))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd"))))
;;  :hook
;;  (python-mode . #'eglot-ensure)
;;  ('c++-mode-hook . #'eglot-ensure)
;;  ('c-mode-hook . #'eglot-ensure))

(setq eldoc-echo-area-use-multiline-p nil)

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(add-hook 'python-mode-hook (lambda () fill-column 120))

(use-package vterm
  :ensure t
  :bind (("C-c v" . vterm)))

(unless (member "Symbols Nerd Font Mono" (font-family-list))
  (nerd-icons-install-fonts))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(nerd-icons-dired vterm ef-themes csv-mode company-box company poetry python-mode magit helpful ivy-rich which-key rainbow-delimiters doom-modeline all-the-icons gruvbox-theme counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
