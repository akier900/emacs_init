;; org mode setup
(require 'org)


;; create intermediate states for TODO items
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "REVIEW" "DONE")))

;; make bold font red in org-mode since sometimes fontification seems lacking
(add-to-list 'org-emphasis-alist
	     '("*" (:foreground "red")))

(add-to-list 'org-emphasis-alist
	     '("/" (:foreground "yellow")))

;; stop asking for confirmation everytime I wanna eval some src code block
(setq org-confirm-babel-evaluate nil)


;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; hide emphasis marks.
(setq org-hide-emphasis-markers t)

(require 'org-gcal)
(require 'org-make-toc)

;; org-rich-yank setup
(require 'org-rich-yank)
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-M-y") #'org-rich-yank))

	 
;; org-superstar (org-bullets descendant
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))



;; Active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)	
   (emacs-lisp . t)
   (python . t)
   (C . t)
   (octave . t)))

