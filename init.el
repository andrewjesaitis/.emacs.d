;;;;
;; Packages
;;;;
;;b(server-start)
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


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; colorful parenthesis matching
(use-package rainbow-delimiters
  :ensure t
  :config
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
  :config
  (add-hook 'prog-mode-hook #'global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(json-jsonlist))))
(use-package evil-escape
  :ensure t)
(use-package general
  :ensure t)
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (add-hook 'after-init-hook 'which-key-mode))
(use-package evil-org
  :ensure t)
(use-package counsel
  :ensure t)
(use-package org-plus-contrib
  :ensure t)
(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'company-mode))
(use-package ag
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package elpy
  :ensure t)
(use-package scss-mode
  :ensure t)
(use-package ws-butler
  :ensure t)

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

