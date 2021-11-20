;;; package -- Summary



;;; commentary

;;; code

;; helm-projectile setup
;(require 'helm-projectile)
;(helm-projectile-on)


;; other options are
;; default, recentf, recently-active, access-time
;(setq projectile-sort-order 'modification-time)

;; open top level in dired once project has been selected
;(setq projectile-switch-project-action #'projectile-dired)
;(setq projectile-find-dir-includes-top-level 1)

;; make projectile use better fd command (available thru chocolatey
;(setq projectile-generic-command "fd . -type f -print0")




;; ;; org-capture
;; (setq org-directory "c:/Users/erici/org-mode")
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
;; ;; org-projectile
;; (require 'org-projectile)
;; (setq org-projectile-projects-file
;;       "c:/Users/erici/project_todos.org") ; path for windows surface
;; (push (org-projectile-project-todo-entry) org-capture-templates)
;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
;; (global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)

;; ;; make project headings into links
;; (setq org-confirm-elisp-link-function nil)
;; ;;; setup-projectile.el ends here



















