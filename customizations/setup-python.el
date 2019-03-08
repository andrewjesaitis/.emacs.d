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
