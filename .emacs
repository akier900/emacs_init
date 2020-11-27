;;; Keybindings

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

;; setup for straight.el. This allows moving between computers and
;; automatic installation of missing packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)      
 

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
(straight-use-package 'org-bullets)
(straight-use-package 'helm)
(straight-use-package 'magit)
(straight-use-package 'recentf)
(straight-use-package 'recentf-ext)
(straight-use-package 'verilog-mode)
(straight-use-package 'yasnippet-snippets)
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
(straight-use-package 'sr-speedbar)
(straight-use-package 'company)
(straight-use-package 'company-math)
(straight-use-package 'company-c-headers)
(straight-use-package 'company-jedi)
(straight-use-package 'cc-mode)
(straight-use-package 'header2)
(straight-use-package 'auto-complete-sage)
(straight-use-package 'helm-sage)
(straight-use-package 'sage-shell-mode)
(straight-use-package 'sublime-themes)
(straight-use-package 'nyan-mode)
(straight-use-package 'dumb-jump)

;; Enable dired-x for additional dired commands
(require 'dired-x)
(require 'sage-shell-mode)

;; require files for auto-completion "as you type"
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)

;; This is only needed once near the top of file. This sets up use of package 'use-package'
(eval-when-compile
  (require 'use-package))



;; Bind M-/ to M-x hippie-expand instead of dabbrev
(global-set-key "\M- " 'hippie-expand)


;; display tooltips in echo area instead of seperate frame
(tooltip-mode -1)
(setq tooltip-use-echo-area t) 


;; associate .txt files with asciidoc mode
(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))


(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))


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




;; global shortcut key for magit-status command
(global-set-key (kbd "C-x g") 'magit-status)

;; allow info to visit links to 'gitman' info manual
(setq magit-view-git-manual-method 'man)



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


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight semi-bold :height 98 :width normal))))
 '(cursor ((t (:background "orange1")))))

;; Trigger an insertion of relevant text depending on mode of

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(asm-comment-char 64)
 '(custom-enabled-themes (quote (spolsky)))
 '(custom-safe-themes
   (quote
    ("b267f390ae9919ae61fd6b9973971585ed71bc069a08196b136fd0389d4bc12b" "f6f566f0c8e76d5339bd9368b816a7e57f4fb318e9795033e47d60e8dc121bf2" "730277652b2e8eeb072604bc779a5782f7a4fbc0cf7803c69601b4be8a681d87" "dba1b403539040029374556514170fab030572b7a99d031cb0c29deca5872524" "8a881af89b6790a905bae2f11bb0b93010ebcd010bdc79104087aef77b22d8d7" "35ce30aa61c3d288dfb6f0687420d8773c6281e77cf07dc9dc9e9e9c315d29ae" "ff9df472cd58c2c226e8a11c36b2bc88e95eeb1666740ecb46e0155ce55073af" "03db8a813340989af5bd1bc24578d5e0cac295dcaaf30dc9546891bea0249900" "f2a626e8b41f12afbf3acc081dde9387b85b80525dbc70e9b61850a774c37e7a" "8da4938e0e5754d199ef23087edbddfadf78ecbacbd49b6c766d64296310e3e3" "9e009e887a64cffcb6e51946a63562ccbb3b177a8cd285571a5737757793baf5" "84c307eb4d445f8cff00eb315939652c8cfa7d1e08cc16861df8fdd2c07b66ff" default)))
 '(display-line-numbers t)
 '(gas-comment-char 59)
 '(inhibit-startup-screen t)
 '(linum-format " %7i ")
 '(nyan-mode t)
 '(show-paren-mode t)
 '(user-full-name "Eric Aki, Email: Akier@pdx.edu"))

;; when 'M-x check-parens' is run, it will highlight ungrouped expression from
(setq show-paren-style 'expression)



;; Add hook for assembly files with .s extenstion to trigger GAS mode.
;; (require 'gas-mode)					   ;;
;;  (add-to-list 'auto-mode-alist '("\\.s\\'" . gas-mode)) ;;

;; Tell emacs where personal elisp lib dir is
(add-to-list 'load-path "~/.emacs.d/lisp/")

(load "nyan-mode")



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

(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)

(autoload 'auto-make-header "header2")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook 'auto-make-header)


;; Define keyboard shortcut for M-x recompile
(global-set-key (kbd "C-c C-r") 'recompile)


;; Enable auto-completion in IELM: Emacs REPL
;; Enter IELM with M-x ielm
(defun ielm-auto-complete ()
  "Enables 'auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
		     ac-source-variables
		     ac-source-features
		     ac-source-symbols
		     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; Improve system default commmands with their helm counterparts
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)


;; ;; helm-gtags setup
;; (setq
;;  helm-gtags-ignore-case t
;;  helm-gtags-auto-update t
;;  helm-gtags-use-input-at-cursor t
;;  helm-gtags-pulse-at-cursor t
;;  helm-gtags-prefix-key "\C-cg"
;;  helm-gtags-suggested-key-mapping t
;;  )

;; (require 'helm-gtags)
;; ;; Enable helm-gtags-mode
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)







;; If you want to enable inline display of LaTeX outputs only,
;; uncomment the following line.
;; (setq sage-shell-view-default-commands 'output)

;; If you want to enable inline display of plots only,
;; uncomment the following line.
;; (setq sage-shell-view-default-commands 'plot)


;; Run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
(sage-shell:define-alias)

(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)


;; Activation for dumb-jump package
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
