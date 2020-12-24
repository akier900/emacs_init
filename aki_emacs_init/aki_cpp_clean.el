 ;;;; BASICS (package independent) ============================================================
;;;Code:

;; add melpa repo
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; refresh packages everytime we open emacs (hopefully)
(add-hook 'after-init-hook 'package-refresh-contents)


;; line numbers always!
(global-display-line-numbers-mode)


;; use better default font
(set-frame-font "Hack 11" nil t)



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
(straight-use-package 'inkpot-theme)
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
(straight-use-package 'yasnippet-classic-snippets)
(straight-use-package 'company)		;"complete-anything" when it works..
(straight-use-package 'company-box)	;cool box popups for completions
(straight-use-package 'company-math)
(straight-use-package 'company-statistics)
(straight-use-package 'company-c-headers)
(straight-use-package 'company-native-complete)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'helm-lsp)
(straight-use-package 'dap-mode)	;debugger support. Probably need more config
(straight-use-package 'ccls)		;c++ language server. configured with lsp-mode
(straight-use-package 'eglot)
(straight-use-package 'helm-company)
(straight-use-package 'which-key)
(straight-use-package 'modern-cpp-font-lock)
(straight-use-package 'smartparens)	;smart parentheses
(straight-use-package 'smart-semicolon)
(straight-use-package 'tiny)
(straight-use-package 'flycheck)
(straight-use-package 'company-quickhelp)

;; flycheck setup
(global-flycheck-mode)

;; smart-semicolon for programming modes
(add-hook 'prog-mode-hook #'smart-semicolon-mode)


;; use better sublime-theme (need this after installing sublime-themes)
(load-theme 'inkpot t)



 
;; lsp-mode-setup
(require 'lsp-mode)
(add-hook 'c++-mode-hook #'lsp)



;; lsp-treemacs
(lsp-treemacs-sync-mode 1)

;; helm and lsp-helm setup
(require 'helm)
(require 'helm-config)

(add-hook 'after-init-hook 'global-company-mode)


(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; improve system default commands with helm counterparts
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; lsp mode 

(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)


;; company box mode
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

(setq company-show-numbers t)
(company-quickhelp-mode)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)		;Default is 0.2


;; which-key setup
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
(setq lsp-keymap-prefix "C-c l")
     

(require 'ccls)



;; yasnippet setup
(require 'yasnippet)
(yas-global-mode 1)


;; function for easy compilation of  C/C++ files using f5
(defun code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
	 (let ((file (file-name-nondirectory buffer-file-name)))
	   (format "%s -o %s %s"
		   (if (equal (file-name-extension file) "cpp") "g++" "gcc" )
		   (file-name-sans-extension file)
		   file)))
    (compile compile-command)))

;; set function to f5 shortcut
(global-set-key [f5] 'code-compile)


