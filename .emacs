;;; Keybindings



;; Set keybinding for toggling truncation. Something I need
;; far too often...
(global-set-key (kbd "C-c 4") 'toggle-truncate-lines)

;; keybinding for eval-buffer to <F6>
(global-set-key (kbd "<f6>") 'eval-buffer)


;; map alt-o to switch between open windows
(global-set-key (kbd "M-o") 'other-window)

;; setup for straight.el. This allows moving between computers and
;; automatic installation of missing packages
      
 

;; This bootstrap snippet allows straignt.el to install itself with some magic.
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

;; install all necessary packages via straight.el package
(straight-use-package 'use-package)
(straight-use-package 'el-patch)
(straight-use-package 'org)
(straight-use-package 'org-ac)
(straight-use-package 'org-bullets)
(straight-use-package 'helm)
(straight-use-package 'slack)
(straight-use-package 'magit)
(straight-use-package 'recentf)
(straight-use-package 'recentf-ext)
(straight-use-package 'adoc-mode)
(straight-use-package 'verilog-mode)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'ac-octave)
(straight-use-package 'flyparens)
(straight-use-package 'auctex)
(straight-use-package 'auto-complete-auctex)
(straight-use-package 'flymake)
(straight-use-package 'whole-line-or-region)
(straight-use-package 'git)
(straight-use-package 'helm-org)
(straight-use-package 'helm-c-moccur)
(straight-use-package 'helm-c-yasnippet)
(straight-use-package 'helm-bibtex)
(straight-use-package 'helm-bibtexkey)
(straight-use-package 'spacemacs-theme)
(straight-use-package 'function-args)
(straight-use-package 'ggtags)
(straight-use-package 'sr-speedbar)
(straight-use-package 'company)
(straight-use-package 'company-math)
(straight-use-package 'company-c-headers)
(straight-use-package 'cc-mode)
(straight-use-package 'helm-gtags)





;; require files for auto-completion "as you type" 
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)

;; This is only needed once near the top of file. This sets up use of package 'use-package'
(eval-when-compile
  (require 'use-package))


;; helps combat screen tearing in exchange for reduced performance
;; speed
(setq redisplay-dont-pause t)



;; Bind M-/ to M-x hippie-expand instead of dabbrev
(global-set-key "\M- " 'hippie-expand)


;; display tooltips in echo area instead of seperate frame
(tooltip-mode -1)
(setq tooltip-use-echo-area t) 


;; Maximize window upon opening of emacs

(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))
(add-hook 'window-setup-hook 'maximize-frame t)



;; associate .txt files with asciidoc mode
(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))


(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))


;; allow switching of windows with combo of shift and arrow key
(windmove-default-keybindings)


;; make pretty utf-8 style bullets in org mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; make helm configuration file neccesary
(require 'helm-config)


;; enable IDO mode for file and directory finding as well as buffer switching
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)



;; adding hooks for associating octave with .m files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Make Octave default mode for .m files and set comment chars to % instead for cross-compatibility
 (add-hook 'octave-mode-hook		
    (lambda () (progn (setq octave-comment-char ?%)
                      (setq comment-start "% ")
                      (setq comment-add 0))))


;; Turn on abbrevs, auto-fill and font-lock features for octave mode
;; automatically.

(add-hook 'octave-mode-hook
	  (lambda ()
	    (abbrev-mode 1) 
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))




;(slack-register-team
; :name "nexgarden"
; :token (auth-source-pick-first-password
;	 :host "nexgarden.slack.com"
;	 :user "akier@pdx.edu")
; :subscribed-channels '((channel1 channel2)))
;     



;; global shortcut key for magit-status command
(global-set-key (kbd "C-x g") 'magit-status)

;; allow info to visit links to 'gitman' info manual
(setq magit-view-git-manual-method 'man)



;; If you want to enable inline display of LaTeX outputs only,
;; uncomment the following line.
;;(setq sage-shell-view-default-commands 'output)

;; If you want to enable inline display of plots only,
;; uncomment the following line.
;; (setq sage-shell-view-default-commands 'plot)


;; C-c c for asynchronous evaluating (only for SageMath code blocks).
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))

;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; Do not evaluate code blocks when exporting.
(setq org-export-babel-evaluate nil)

;; Show images when opening a file.
(setq org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; recentf package setup
(require 'recentf)

;; get rid of 'find-file-read-only' and replace with something more
;; useful
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode
(recentf-mode t)

;; 50 files should be more than enough
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use 'ido-comleting-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; force Ido to always create a new buffer (in C-x b) if name
;; does not already exist.
(setq ido-create-new-buffer 'always)


;; Wait till package is loaded (after init is read) then
;; load theme
(add-hook 'after-init-hook (lambda () (load-theme 'spacemacs-dark)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 113 :width normal))))
 '(cursor ((t (:background "orange1")))))

;; Trigger an insertion of relevant text depending on mode of
;; new file in current buffer. system default
(add-hook 'find-file-hook 'auto-insert)


;; Enable YAsnippets. Add user snippets to ~/.emacs.d/snippets
;; or invoke M-x-yas-new-snippet
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; spell checking for latex docs
(add-hook 'tex-mode-hook
  #'(lambda () (setq ispell-parser 'tex)))






 '(straight-use-package-by-default t)
 ;; tells use-package to defer to straight.el as it's default package manager for installs.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asm-comment-char 64)
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ab2cbf30ab758c5e936b527377d543ce4927001742f79519b62c45ba9dd9f55e" default)))
 '(display-line-numbers t)
 '(gas-comment-char 59)
 '(helm-gtags-prefix-key "\\C-t")
 '(helm-gtags-suggested-key-mapping t)
 '(inhibit-startup-screen t)
 '(nyan-mode t)
 '(show-paren-mode t))

;; when 'M-x check-parens' is run, it will highlight ungrouped expression from
;; where your cursor currently
(show-paren-mode t)
(setq show-paren-style 'expression)



;; Add hook for assembly files with .s extenstion to trigger GAS mode.
;; (require 'gas-mode)					   ;;
;;  (add-to-list 'auto-mode-alist '("\\.s\\'" . gas-mode)) ;;

;; Tell emacs where personal elisp lib dir is
(add-to-list 'load-path "~/.emacs.d/lisp/")

(load "nyan-mode")



;; for this to load properly, must clone repo into .emacs.d
;(add-to-list 'load-path "~/.emacs.d/function-args")
(require 'function-args)

;; automaticaly active function-args-mode for each file
;; This gives additional functionality for editing C/C++ files. 
(fa-config-default)


;; ggtags keybindings
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)


;; Integrate imenu with ggtags. provides interface.
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)






;; Allow speedbar static file view to show all files, not just those
;; in the current directory
(setq speedbar-show-unknown-files t)


;; Setup semantic code completion
(require 'cc-mode)
(require 'semantic)

;; enable minor modes upon startup
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)


;; auto enable gdb to operate in seperate buffers
(setq
 gdb-many-windows t
 ;; non-nil means display source file containing the main routine at startup
 gdb-show-main t)



;; set asm-mode indenting back to "normal" behavior
(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq tab-always-indent (default-value 'tab-always-indent)))

(add-hook 'asm-mode-hook #'my-asm-mode-hook)
