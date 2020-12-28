;;;; BASICS (package independent) ============================================================
;;;Code:

;; add melpa repo
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))



;; refresh packages everytime we open emacs (hopefully)
(unless package-archive-contents
  (package-refresh-contents))


;; line numbers always!
(global-display-line-numbers-mode)





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

(setq straight-allow-recipe-inheritance nil)

;; install pacakges (general)
;; themes
;(straight-use-package 'doom-themes)
(straight-use-package 'inkpot-theme)
(straight-use-package 'sublime-themes)

;; git, helm, hippie expand
(straight-use-package 'magit)
(straight-use-package 'git)
(straight-use-package 'helm)
(straight-use-package 'hippie-exp-ext)
(straight-use-package 'hippie-namespace)

;; packages for programming
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-classic-snippets)
(straight-use-package 'projectile)

;; company general packages
(straight-use-package 'company)		;"complete-anything" when it works..
(straight-use-package 'company-box)	;cool box popups for completions
(straight-use-package 'company-math)
(straight-use-package 'company-statistics)
(straight-use-package 'company-c-headers)
(straight-use-package 'company-native-complete)

;; pretty icon packages
(straight-use-package 'treemacs)
(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-dired)

;; misc
(straight-use-package 'helm-company)
(straight-use-package 'which-key)	;helps to keep track of keybindings.
(straight-use-package 'smartparens)	;smart parentheses
(straight-use-package 'smart-semicolon)
(straight-use-package 'tiny)
(straight-use-package 'flycheck)
(straight-use-package 'company-quickhelp)
(straight-use-package 'cpp-auto-include) ;auto add necessary c++ header files

;; lsp-mode packages (fuck irony-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'helm-lsp)
(straight-use-package 'dap-mode)
(straight-use-package 'ccls)


;; Specific Helm mode packages
(straight-use-package 'helm-company)
(straight-use-package 'helm-file-preview)
(straight-use-package 'helm-flycheck)
(straight-use-package 'helm-gtags)
(straight-use-package 'helm-org)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-ag)

		      
;; use theme (needs to be after themes are installed
(load-theme 'leuven t)

;; use better default font
(set-frame-font "Fira Code Retina 11" nil t)


;; keybindings for company mode
;; make it so we can navigate completion popups with C-n and C-p
;; instead of default M-n and M-p
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))


;; lsp-mode-setup/ dap-mode-setup
(require 'lsp-mode)

;; helm-lsp
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

;; dap-mode
(require 'ccls)
(require 'dap-cpptools)

;; active hydra whenever program hits breakpoint
(add-hook 'dap-stopped-hook (lambda (arg)
				    (call-interactively #'dap-hydra)))
	 


;; hdl/verilog setup (hdl_checker must be in path and is installed by pip)
(custom-set-variables
 '(lsp-vhdl-sever 'hdl-checker))
(add-hook 'verilog-mode-hook 'lsp)
(add-hook 'vhdl-mode-hook 'lsp)


;; c/c++ setup
;; hooks for lsp-mode
(add-hook 'c++-mode-hook  'lsp)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'objc-mode-hook 'lsp)



;; flycheck setup
(global-flycheck-mode)



;; smart-semicolon for programming modes
(add-hook 'prog-mode-hook #'smart-semicolon-mode)



;;;; HELM SETUP start
(require 'helm)
(require 'helm-config)

;; use helm for finding other buffers
;; improve system default commands with helm counterparts
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; helm-swoop
(global-set-key (kbd "M-i") 'helm-swoop)


;;helm file preview setup
(require 'helm-file-preview)
(helm-file-preview-mode 1)


;; helm-flycheck setup
(require 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; helm/gtags setup
(load (aki-get-fullpath "setup-helm-gtags"))			;change path for specific machine


;;  helm-org setup
;(add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
;(add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))

;; helm-projectile setup
(require 'helm-projectile)
(helm-projectile-on)

;;;; HELM SETUP end

;;;; COMPANY MODE SETUP start
;; Company mode enable
(add-hook 'after-init-hook 'global-company-mode)

;; company box mode
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)


(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))


;; better settings
(setq company-show-numbers t)
(company-quickhelp-mode)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)		;Default is 0.2

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/10.2.0") ;only valid for arch on desktop
;; company mode setup end

;; projectile setup
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)





;; all the icons
(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))




;; yasnippet setup
(require 'yasnippet)
(yas-global-mode 1)



(defun code-compile ()
  (interactive)
  (unless (or (file-exists-p "Makefile")
	      (file-exists-p "makefile"))
    (set (make-local-variable 'compile-command)
	 (let ((file (file-name-nondirectory buffer-file-name)))
	   (format "%s  %s -o  %s -Wall -std=c++11 "
		 (if (equal "cpp" "cpp") "g++" "gcc")
		 file (file-name-sans-extension file))))
		 (compile compile-command)))


;; set function to f5 shortcut
(global-set-key [f5] 'code-compile)


;; (defun code-run ()
;;   "This function instead runs the code using the same basic function
;; structure as code-compile. The file is assumed to have the same base
;; name but with extension .exe. For example: foo.cpp is compiled and
;; executable is assumed to be foo.exe"
;;   (interactive)
;;   (set (make-local-variable 'compile-command)
;; 	(concat (file-name-sans-extension buffer-file-name)
;; 		".exe"))
;;   (compile compile-command))

;; ;; set code-run to f5's neighbor
;; (global-set-key [f6] 'code-run)

;;; aki_cpp_clean.el ends here
