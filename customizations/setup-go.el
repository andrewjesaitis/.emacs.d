;; Packages for writing go
(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (setq gofmt-command "goimports"))


;; (use-package company-go
;;   :ensure t)
;; (use-package flycheck-gometalinter
;;   :ensure t)
;; (use-package godoctor
;;   :ensure t)
;; (use-package go-eldoc
;;       :ensure t
;;       :config
;;       (add-hook 'go-mode-hook 'go-eldoc-setup))
