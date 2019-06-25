;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; ivy
(ivy-mode 1)
(setq projectile-completion-system 'ivy)
(setq magit-completing-read-function 'ivy-completing-read)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (t . ivy--regex-ignore-order)))
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "C-c c") 'counsel-compile)

;; projectile everywhere!
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
   "l" 'counsel-locate
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
