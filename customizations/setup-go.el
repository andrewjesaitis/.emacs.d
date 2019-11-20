;; Packages for writing go
(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save))
  :config
  (setq tab-width 4
        gofmt-command "goimports")
(use-package company-go
  :ensure t)
(use-package flycheck-gometalinter
  :ensure t)
(use-package godoctor
  :ensure t)
