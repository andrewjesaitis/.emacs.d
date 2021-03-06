;;; Org Mode
(use-package org
  :mode
  ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)
  :ensure t
  :config
  (require 'org-checklist)
  (local-set-key (kbd "<SPC>") nil)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (setq org-agenda-files (quote ("~/Dropbox/org"))
        org-tags-match-list-sublevels (quote indented))
)

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-set-initial-state 'org-agenda-mode 'normal)
  )

(use-package org-plus-contrib
  :ensure t)

(use-package evil-org
  :ensure t)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Custom Key Bindings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'avj/org-todo)
(global-set-key (kbd "<S-f5>") 'avj/widen)
(global-set-key (kbd "<f7>") 'avj/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'avj/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'avj/hide-other)
(global-set-key (kbd "<f9> n") 'avj/toggle-next-task-display)
(global-set-key (kbd "<f9> r") 'boxquote-region)

(global-set-key (kbd "<f9> t") 'avj/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'avj/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "C-s-<f12>") 'avj/save-then-publish)

;; TODO: Org-mode specific keymaps using general
(general-define-key
 :states 'motion
 :keymaps 'org-mode-map
 :prefix "SPC"
   "" nil
 
   "otd"  'org-deadline
   "oe"   '(:ignore t :which-key "Effort")
   "oes"  'org-set-effort
   "op"   'org-priority
   "or"   'org-refile
   "ots"  'org-schedule
   "ot"   '(:ignore t :which-key "Timestamp")
   "ota"  'org-time-stamp
   "oti"  'org-time-stamp-inactive
   "ote"  'org-evaluate-time-range
   "ox"   'org-toggle-checkbox
   "os"   '(:ignore t :which-key "todo State")
   "os"   'org-todo
   "oss"  'avj/org-todo-done
   "oz"   'org-archive-subtree)

(general-define-key
 :states 'motion
 :keymaps 'org-agenda-mode-map
 :prefix "SPC"
   "" nil
 
   "od"  'org-agenda-deadline
   "of"  '(:ignore t :which-key "Filter")
   "oft" 'org-agenda-filter-by-tag
   "ofc" 'org-agenda-filter-by-category
   "ofr" 'org-agenda-filter-by-regexp
   "ofe" 'org-agenda-filter-by-effort
   "ofc" 'org-agenda-filter-remove-all
   "on"  'org-agenda-capture
   "op"  'org-agenda-priority
   "or"  'org-agenda-refile
   "os"  'org-agenda-schedule
   "os"  '(:ignore t :which-key "todo State")
   "ost" 'org-todo
   "oss" 'avj/org-todo-done)

(defun avj/org-todo-done ()
  (interactive)
  (org-todo "DONE"))

(defun avj/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun avj/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

;; TODO states
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              )))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("NEXT" ("WAITING") ("CANCELLED"))
              ("DONE" ("WAITING") ("CANCELLED")))))

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/inbox.org")


;; Capture templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org/inbox.org")
               "* TODO %?\n%U\n")
              ("n" "note" entry (file "~/Dropbox/org/inbox.org")
               "* %? :NOTE:\n%U\n"))))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))


;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun avj/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'avj/verify-refile-target)

;; Agenda Views
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("m" "My View"
               ((agenda "" nil)
                (tags "INBOX"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'avj/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'avj/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if avj/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'avj/skip-projects-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-INBOX-CANCELLED-WAITING/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if avj/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'avj/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-INBOX-CANCELLED-WAITING/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if avj/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'avj/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if avj/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'avj/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled avj/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines avj/hide-scheduled-and-waiting-next-tasks)))
                (tags "-INBOX/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'avj/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

(defun avj/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'avj/org-auto-exclude-function)
(setq org-stuck-projects (quote ("" nil nil "")))

;; Agenda Functions
(defun avj/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun avj/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (avj/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun avj/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun avj/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun avj/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun avj/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar avj/hide-scheduled-and-waiting-next-tasks t)

(defun avj/toggle-next-task-display ()
  (interactive)
  (setq avj/hide-scheduled-and-waiting-next-tasks (not avj/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if avj/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun avj/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (avj/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun avj/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (avj/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (avj/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun avj/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (avj/list-sublevels-for-projects-indented)
  (if (save-excursion (avj/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((avj/is-project-p)
            nil)
           ((and (avj/is-project-subtree-p) (not (avj/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun avj/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((avj/is-task-p)
        nil)
       (t
        next-headline)))))

(defun avj/skip-project-trees()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((avj/is-project-p)
        subtree-end)
       (t
        nil)))))

(defun avj/skip-projects-and-single-tasks ()
  "Skip trees that are projects, tasks that are single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((and avj/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((avj/is-project-p)
        next-headline)
       ((and (avj/is-task-p) (not (avj/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun avj/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((avj/is-project-p)
        next-headline)
       ((and (not limit-to-project)
             (avj/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (avj/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun avj/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((avj/is-project-p)
        subtree-end)
       ((avj/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun avj/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((avj/is-project-p)
        next-headline)
       ((and (avj/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (avj/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun avj/skip-projects-and()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((avj/is-project-p)
        subtree-end)
       (t
        nil)))))

(defun avj/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (avj/is-subproject-p)
        nil
      next-headline)))

(defun avj/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

;; Tags
; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@home" . ?H)
                            ("@boat" . ?B)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("PERSONAL" . ?P)
                            ("ZYMERGEN" . ?Z)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
