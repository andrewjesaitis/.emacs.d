;; Customizations relating to editing a buffer.
 (use-package evil
   :ensure t
   :init
   (setq evil-overriding-maps nil
         evil-intercept-maps nil
         evil-insert-state-bindings nil
         ;; must be set before loading evil no matter what
         evil-want-keybinding nil
         ;; required for gn
         evil-search-module 'evil-search
         evil-ex-search-persistent-highlight nil
         ;; Y like D
         evil-want-Y-yank-to-eol t
         evil-escape-key-sequence "hh"
         evil-escape-delay 0.2)
   :config
   (evil-mode)
   (evil-escape-mode)
   (setq evil-want-fine-undo t))

 (use-package evil-collection
   :ensure t
   :after evil
   :config
   (evil-collection-init)
   )

(use-package evil-colemak-basics
  :ensure t
  :after (:all evil-collection evil))

;; for some reason this form can't be in the config for the package
(global-evil-colemak-basics-mode 1)

;; CUSTOMIZATIONS ::
;; Use a bar cursor
(set-default 'cursor-type 'bar)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (read-kbd-macro "<C-RET>") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; Line width to 99 characters
(setq fill-column 99)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  ;; keep track of saved places in ~/.emacs.d/places
  (setq save-place-file (concat user-emacs-directory "places")))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; yasnippet
;; http://www.emacswiki.org/emacs/Yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq electric-indent-mode nil)

;; Move lines up and down

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)


;; Trim touched white-space on save in programming buffers
(use-package ws-butler
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))
