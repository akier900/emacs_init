;; settings for external packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; myPackages contains a list of package names  

;(defvar myPackages
;  '(
;    material-theme    			;theme
;    ))

;; scans myPackages. if not installed, do it now!
;(mapc #'(lambda (package)
;	  (unless (package-installed-p package)
;	    (package-install package)))
;     myPackages)


;; setup for straight.el

;; bootstrap snippet allows straight.el to install itself
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


'(straight-use-package-by-default t)

;; install all necessary packages via straight.el 
(straight-use-package 'anaconda-mode) 
(straight-use-package 'neotree)		;like a speedbar but better
(straight-use-package 'doom-themes)	;pretty themes
(straight-use-package 'use-package)	;package management alternative
(straight-use-package 'magit)		;magit, git interface
(straight-use-package 'flyparens)	;helps match parentheses by on-the-fly checking
(straight-use-package 'flymake)		;syntax checking
(straight-use-package 'corral)		;Helps wrap things in delimiters
(straight-use-package 'git)		;git
(straight-use-package 'smartparens)	;another parentheses helper
(straight-use-package 'hippie-exp-ext)
(straight-use-package 'hippie-namespace)
(straight-use-package 'helm)
(straight-use-package 'yasnippet)
(straight-use-package 'company)		;"complete-anything" when it works..
(straight-use-package 'company-anaconda) ;completion for python-environments
(straight-use-package 'company-jedi)	
(straight-use-package 'company-ctags)	
(straight-use-package 'company-native-complete) 
(straight-use-package 'company-shell)   ;shell completions
(straight-use-package 'company-box)	;cool box popups for completions
(straight-use-package 'company-tabnine)	;AI completion (helps a lot)
(straight-use-package 'lsp-mode)	;
(straight-use-package 'elpy)
(straight-use-package 'ein)
(straight-use-package 'flycheck)	;use only arch/linux
(straight-use-package 'python-info)
(straight-use-package 'py-autopep8)




;; ======================================================================
;; package specific keybindings (minor mode dependent)
;; ======================================================================


;; corral auto delimeter hotkeys
(global-set-key (kbd "M-9") 'corral-parentheses-backward)
(global-set-key (kbd "M-0") 'corral-parentheses-forward)
(global-set-key (kbd "M-[") 'corral-brackets-backward)
(global-set-key (kbd "M-]") 'corral-brackets-forward)
(global-set-key (kbd "M-{") 'corral-braces-backward)
(global-set-key (kbd "M-}") 'corral-braces-forward)
(global-set-key (kbd "M-\"") 'corral-double-quotes-backward)


;; smartparens setup
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)




;; hippie-expand setup
(require 'hippie-namespace)
(global-hippie-namespace-mode 1)


;; helm setup
(require 'helm)
(require 'helm-config)


(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))


;; Improve system default commands with helm counterparts
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)







;; magit setup
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-view-git-manual-method 'man)



;; Org Mode ===========================================
;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; Do not evaluate code blocks when exporting.
(setq org-export-babel-evaluate nil)

;; Show images when opening a file.
(setq org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)




;; yasnippet
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)




;; ;; only leave uncommented if working in linux/arch
;; ;; Enable Flycheck
;(when (require 'flycheck nil t)
;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;  (add-hook 'elpy-mode-hook 'flycheck-mode))

(global-flycheck-mode)

;; ;; Enable autopep8 autoformatter
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)









