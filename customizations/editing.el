;; Customizations relating to editing a buffer.

;; evil mode
(setq evil-overriding-maps nil
      evil-intercept-maps nil
      evil-pending-intercept-maps nil
      evil-pending-overriding-maps nil
      evil-want-keybinding nil
      evil-escape-key-sequence "nn"
      evil-escape-delay 0.2)
(when (require 'evil-collection nil t)
  (evil-collection-init))
;; subvert evil-operation.el overrides (dired, ibuffer etc.)
(require 'evil)
(evil-mode 1)
(evil-escape-mode 1)
(setq evil-want-fine-undo t)
(advice-add 'evil-make-overriding-map :override #'ignore)
(advice-add 'evil-make-intercept-map  :override #'ignore)
(advice-add 'evil-add-hjkl-bindings   :override #'ignore)
(require 'evil-colemak-basics)
(global-evil-colemak-basics-mode)
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
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; yasnippet
;; http://www.emacswiki.org/emacs/Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

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

;; save and rename current buffer
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Trim touched white-space on save in programming buffers
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)
