;;;;
;; Packages
;;;;
(server-start)
(setq load-prefer-newer t)
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; update melpa metadata.
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-defer t)

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

;; colorful parenthesis matching
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; edit html tags like sexps
(use-package tagedit)

;; git integration
(use-package magit
  :ensure t)
(use-package git-gutter
  :ensure t)
(use-package flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'global-flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(json-jsonlist))))
(use-package company
  :ensure t
  :diminish
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay .5
        company-dabbrev-downcase nil))
(use-package markdown-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package scss-mode
  :ensure t)

(use-package lsp-mode
  :hook
  (go-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  :config
  (lsp-enable-which-key-integration)
  :commands
  (lsp  lsp-deferred))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 1
        lsp-ui-sideline-delay 2))

(use-package company-lsp
  :ensure t
  :after lsp-mode company-mode
  :commands company-lsp)

(use-package lsp-ivy
  :ensure t
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  '(require dap-go)
  (dap-go-setup))

(evil-define-key 'normal lsp-mode-map (kbd "<SPC>l") lsp-command-map)
;; (general-define-key
;;  :states 'motion
;;  :keymaps 'lsp-mode-map
;;  :prefix "SPC"
;;    ""     nil

;;    "ld"   #'lsp-describe-thing-at-point
;;    "lr"   #'lsp-rename
;;    "lf"   '(:ignore t :which-key "Find ")
;;    "lfr"  #'lsp-find-references
;;    "lfi"  #'lsp-find-implementation
;;    "lft"  #'lsp-find-type-definition)
;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(if (daemonp)
(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f) (load "ui.el")))))
(load "ui.el"))

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; For org-mode using GTD
(load "setup-org.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-css.el")
(load "setup-python.el")
(load "setup-go.el")

;; Use and Load Custom file
(setq custom-file "~/.emacs.d/customizations/custom.el")
;;(load custom-file)
