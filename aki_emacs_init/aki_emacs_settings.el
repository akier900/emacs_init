;; basic stuffs that dont require packages




;; Allows automatic new lines when navigating past
;; 'end' of current buffer with basic movement commands.
(setq next-line-add-newlines t)

;; Set keybinding for toggling truncation. Something I need
;; far too often...
(global-set-key (kbd "C-c 4") 'toggle-truncate-lines)

;; keybinding for eval-buffer to <F6>
(global-set-key (kbd "<f6>") 'eval-buffer)


;; map alt-o to switch between open windows
(global-set-key (kbd "M-o") 'other-window)

;; Get rid of annoying prompt every time I want to follow a symbolic link
(setq vc-follow-symlinks nil)

;;
(setq org-confirm-babel-evaluate nil)



;; display tooltips in echo area instead of seperate frame
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; when 'M-x check-parens' is run, it will highlight ungrouped expression from
(setq show-paren-style 'expression)

;; tell emacs where personal elisp lib dir is
(add-to-list 'load-path "~/.emacs.d/lisp/")


;; enable ido mode for file and directory searching.
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)



;; make shortcut for compilation
(global-set-key (kbd "<f5>") (lambda()
			       (interactive)
			       (setq-local compilation-read-command nil)
			       (call-interactively 'compile)))




;;this snippet needed as of emacs 27.1
;;Code to fix emacs erros that pop up in newest version

(defun load-history-filename-element (file-regexp)
  "Get the first elt of `load-history' whose car matches FILE-REGEXP.
Return nil if there isn't one."
  (let* ((loads load-history)
   (load-elt (and loads (car loads))))
    (save-match-data
      (while (and loads
      (or (null (car load-elt))
          (not (stringp (car load-elt)))
          (not (string-match file-regexp (car load-elt)))))
  (setq loads (cdr loads)
        load-elt (and loads (car loads)))))
    load-elt))










