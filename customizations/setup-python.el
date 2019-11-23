;; Python3
(setq python-shell-interpreter "python3")

;;enable elpy
(elpy-enable)
(setq elpy-rpc-ignored-buffer-size 10024000)
(setq elpy-rpc-large-buffer-size 100000)
;; (defun elpy--xref-backend ()
;;   "Return the name of the elpy xref backend."
;;   (if (or (and (not (elpy-rpc--process-buffer-p elpy-rpc--buffer))
;;                (elpy-rpc--get-rpc-buffer))
;;           elpy-rpc--jedi-available)
;;       'elpy
;;     nil))

(add-hook
 'elpy-mode-hook
 '(lambda ()
    (setq-local ac-max-width 0.5)))
(general-define-key
 :states 'motion
 :keymaps 'python-mode
 :prefix "SPC"
 "" nil
 
 "e"  '(:ignore t :which-key "elpy")
 "en" '(:ignore t :which-key "navigation")
 "end" 'xref-find-definitions
 "enp" 'xref-pop-marker-stack
 "enr" 'xref-find-references
 "ena" 'xref-find-apropos
 "eno" 'elpy-occur-definitions
 "es"  '(:ignore t :which-key "shell")
 "ess" 'elpy-shell-switch-to-shell
 "esl" 'elpy-shell-set-local-shell
 "esk" 'elpy-shell-kill
 "esd" 'elpy-shell-kill-all
 "el"  '(:ignore t :which-key "load-to-shell")
 "ell" 'elpy-shell-send-statement-and-step
 "els" 'elpy-shell-send-statement
 "elg" 'elpy-shell-send-statement-and-go
 "elx" 'elpy-shell-send-statement-and-step-and-go
 "elg" '(:ignore t :which-key "group")
 "elgg" 'elpy-shell-send-group-and-step
 "elgl" 'elpy-shell-send-group
 "elgx" 'elpy-shell-send-group-and-step-and-go
 "elgc" 'elpy-shell-send-codecell-and-step
 "elr" 'elpy-shell-send-region-or-buffer
 "elf" 'elpy-shell-send-defun
 "elc" 'elpy-shell-send-defclass
 ;; TODO: Add other shell-send commands
 "ee" 'elpy-flymake-next-error
 "ep" 'elpy-flymake-previous-error
 "eh" '(:ignore t :which-key "help")
 "ehd" 'elpy-doc
 "ed" '(:ignore t :which-key "debug")
 "edt" 'elpy-pdb-toggle-breakpoint-at-point
 "edb" 'elpy-pdb-debug-buffer
 "edp" 'elpy-pdb-break-at-point
 "ede" 'eply-pdb-debug-last-exception
 "ef" 'elpy-format-code
 "er" 'elpy-refactor
 "ea" 'elpy-multi-edit-python-symbol-at-point)
    
