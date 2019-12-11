;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

;; put path before buffer name when uniquifying a buffer (instead of after)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(use-package recentf
  :ensure t
  :defer 3
  :init
  (setq recentf-save-file (concat user-emacs-directory ".recentf")
        recentf-max-menu-items 40))

;; ivy
(use-package ivy
  :ensure t
  :init
  (setq projectile-completion-system 'ivy
        magit-completing-read-function 'ivy-completing-read
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-ignore-order)))
  :config
  (ivy-mode))

(use-package ivy-hydra
    :ensure t)

(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-<") 'flycheck-previous-error)
(global-set-key (kbd "C->") 'flycheck-next-error)

;; projectile everywhere!
(use-package projectile
  :ensure t)
(projectile-global-mode)
(general-auto-unbind-keys)
(general-define-key
 :states 'motion
 :keymaps 'override
 :prefix "SPC"
   "" nil

   "<SPC>" '(counsel-M-x :which-key "M-x")
   "m" 'magit-status
   "p" 'projectile-command-map
   "s" 'swiper
   "a" 'counsel-ag
   "b" '(:ignore t :which-key "buffer")
   "bb" 'ibuffer
   "bk" 'kill-buffer
   "bn" 'next-buffer
   "bp" 'previous-buffer
   "f" '(:ignore t :which-key "files")
   "ff" 'counsel-find-file
   "fg" 'rgrep
   "fs" 'save-buffer
   "h" '(:ignore t :which-key "help")
   "hd" '(:ignore t :which-key "help-describe")"hdb" 'describe-bindings
   "hdc" 'describe-char
   "hdf" 'counsel-describe-function
   "hdi" 'counsel-info-lookup-symbol
   "hdk" 'describe-key
   "hdp" 'describe-package
   "hdt" 'describe-theme
   "hdv" 'counsel-describe-variable
   "hn" 'view-emacs-news
   "j" '(:ignore t :which-key "jump")
   "jf" 'find-function
   "jv" 'find-variable
   "o" '(:ignore t :which-key "org")
   "oa" 'org-agenda
   "on" 'org-capture
   "q" '(:ignore t :which-key "quit")
   "qQ" 'kill-emacs
   "qs" 'save-buffers-kill-emacs
   "w <S-down>"  'evil-window-move-very-bottom
   "w <S-left>"  'evil-window-move-far-left
   "w <S-right>"  'evil-window-move-far-right
   "w <S-up>"  'evil-window-move-very-top
   "w <down>"  'evil-window-down
   "w <left>"  'evil-window-left
   "w <right>"  'evil-window-right
   "w <up>"  'evil-window-up
   "w" '(:ignore :which-key "window")
   "w-"  'split-window-below
   "w="  'balance-windows
   "wE"  'evil-window-move-very-bottom
   "wF"  'make-frame
   "wH"  'evil-window-move-far-left
   "wN"  'evil-window-move-very-top
   "wO"  'evil-window-move-far-right
   "w\\"  'split-window-right
   "w_"  'split-window-below-and-focus
   "wd" '(:ignore :which-key "delete")
   "wdo" 'delete-other-windows
   "wds" 'delete-window
   "we"  'evil-window-down
   "wf"  'other-frame
   "wh"  'evil-window-left
   "wl"  'follow-mode
   "wn"  'evil-window-up
   "wo"  'evil-window-right
   "ww"  'other-window
   "w|"  'split-window-right-and-focus
   "x TAB" 'indent-rigidly
   "x" '(:ignore t :which-key "text")
   "xU"  'upcase-region
   "xi" 'counsel-unicode-char
   "xa" '(:ignore t :which-key "align")
   "xaa" 'align
   "xac" 'align-current
   "xc"  'count-words-region
   "xd" '(:ignore t :which-key "delete")
   "xdw" 'delete-trailing-whitespace
   "xj" '(:ignore t :which-key "justification")
   "xjc" 'set-justification-center
   "xjf" 'set-justification-full
   "xjl" 'set-justification-left
   "xjn" 'set-justification-none
   "xjr" 'set-justification-right
   "xl" '(:ignore t :which-key "lines")
   "xl" '(:ignore t :which-key "lines")
   "xls" 'sort-lines
   "xt" '(:ignore t :which-key "transpose")
   "xtc" 'transpose-chars
   "xtl" 'transpose-lines
   "xtw" 'transpose-words
   "xu"  'downcase-region)
