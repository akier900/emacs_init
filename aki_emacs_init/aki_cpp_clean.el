;;;; BASICS (package independent) ============================================================
;;;Code:


(setq next-line-add-newlines t)		;auto add new lines at end of buffer
(setq vc-follow-symlinks nil)		;get rid of annoying "follow symlink" warning
(tooltip-mode -1)			;display tooltips in echo area instead of seperate frame
(setq show-paren-style 'expression)
(add-to-list 'load-path "~/.emacs.d/lisp/")


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


;; enable ido mode for file and directory searching
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

;; keybindings
(global-set-key (kbd "M-o") 'other-window)



;;;; PACKAGES ==============================================================================
;; install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))




;; install pacakges (general)
(straight-use-package 'doom-themes)
(straight-use-package 'sublime-themes)
(straight-use-package 'magit)
(straight-use-package 'git)
(straight-use-package 'helm)
(straight-use-package 'hippie-exp-ext)
(straight-use-package 'hippie-namespace)

;; packages for programming
(straight-use-package 'lsp-mode)	;Language Server Protocol integration
(straight-use-package 'lsp-ui)
(straight-use-package 'yasnippet)
(straight-use-package 'company)		;"complete-anything" when it works..
(straight-use-package 'company-box)	;cool box popups for completions
(straight-use-package 'lsp-treemacs)
(straight-use-package 'helm-lsp)
(straight-use-package 'dap-mode)	;debugger support. Probably need more config
(straight-use-package 'ccls)		;c++ language server. configured with lsp-mode
(straight-use-package 'eglot)

;; lsp-mode-setup
(require 'lsp-mode)
(add-hook 'c++-mode-hook #'lsp)

;; lsp-treemacs
(lsp-treemacs-sync-mode 1)

;; lsp-helm
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

(add-hook 'company-mode 'company-box-mode)

(load-theme 'junio t)











