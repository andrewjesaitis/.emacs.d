;;jupyter as an interactive python backend
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)

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

;; (add-hook
;;  'elpy-mode-hook
;;  '(lambda ()
;;     (setq-local ac-max-width 0.5)))

;; Python debugger toggle
(defun toggle-pdb ()
  "Toggle import pbd; pdb.set_trace() at current line"
  (interactive)
  (let ((x (line-number-at-pos))
    (cur (point)))
    (search-forward-regexp "^[ ]*import pdb; pdb.set_trace();")
    (if (= x (line-number-at-pos))
        (let ()
          (move-beginning-of-line 1)
          (kill-line 1)
          (move-beginning-of-line 1))
      (let ()
        (move-beginning-of-line 1)
        (insert "import pdb; pdb.set_trace();\n")))))

(local-set-key (kbd "C-c d") 'toggle-pdb)
