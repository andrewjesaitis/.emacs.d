;; html
(use-package web-mode
  :ensure t
  :mode
  ("\\.js[x]?$" . web-mode)
  :config
  (add-hook 'html-mode-hook #'subword-mode)
  (add-hook 'web-mode-hook #'auto-complete-mode)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))
(use-package tagedit
  :ensure t
  :config
  (add-hook 'html-mode-hook #'tagedit-mode)
  (tagedit-add-paredit-like-keybindings)
  )
(use-package js2-mode
  :ensure t
  :config
  (add-hook 'js-mode-hook #'tern-mode))
(use-package json-mode
  :ensure t)
(use-package tern
  :ensure t
  :config
  (add-hook 'web-mode-hook #'tern-mode))
(use-package tern-auto-complete
  :ensure t
  :config
  (tern-ac-setup))

;; js and jsx
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))
