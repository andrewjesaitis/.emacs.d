;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; themes

(use-package color-theme-sanityinc-tomorrow
  :ensure t)
(use-package leuven-theme
  :ensure t)

(use-package circadian
  :ensure t
  :init
  (setq circadian-themes '(
                           ("7:00" . leuven)
                           ("18:00" . color-theme-sanityinc-tomorrow-bright)
                           ))
  (circadian-setup))

(defun avj/undo-themes (&rest _)
  (mapc #'disable-theme custom-enabled-themes))

(general-add-advice 'load-theme :before #'avj/undo-themes)


;; Font
(set-default-font "Source Code Pro")

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(fringe-mode '(4 . 0))
(global-git-gutter-mode +1)

;; Install icons
(use-package all-the-icons
  :ensure t)

;; Configure the mode line
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode) 
  (doom-modeline-def-segment iconic-emacs-modeline-starter
    (list
     ;; space on left (or - if term)
     mode-line-front-space
     ;; report multilingual input; e.g. U: for utf-8
     mode-line-mule-info
     ;; for emacsclient frame identification
     mode-line-client
     ;; ** if modified, -- if not, %% if RO, %- if RO and modified
     mode-line-modified
     ;; indicates a remote buffer
     mode-line-remote
     mode-line-frame-identification))

  ;; replace modals section; not really a good alternative without defining a
  ;; completely new modeline
  (doom-modeline-def-segment modals
    (doom-modeline-segment--iconic-emacs-modeline-starter))

  (setq global-mode-string
        (list
         ;; current group
         '(:eval (when (fboundp 'fg-mode-line-string)
                   (fg-mode-line-string)))
         ;; show if clocked in to a heading
         '(:eval (when (and (bound-and-true-p org-mode-line-string)
                            (not (string= org-mode-line-string "")))
                   (concat "Clock: <"
                           (string-trim-left org-mode-line-string)
                           "> "))))))

;; typography
(setq-default line-spacing nil)
(setq mac-allow-anti-aliasing t)
(global-prettify-symbols-mode t)

;; Better scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))


;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;;Transparency
;; Make emacs transparent
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Allow transparency to be toggled
 (defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(95 . 95) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)
