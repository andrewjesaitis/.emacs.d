;;jupyter as an interactive python backend
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)

;;enable elpy
(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setq elpy-test-pytest-runner-command (quote ("pytest -n 7 -m \"not slow\"")))
(setq elpy-test-runner (quote elpy-test-pytest-runner))

(add-hook
 'elpy-mode-hook
 '(lambda () (setq-local ac-max-width 0.5)))

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
