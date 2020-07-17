;; setup for straight.el. This allows moving between computers and
;; automatic installation of missing packages


;; This bootsrap snippet allows straignt.el to install itself with some magic.
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
(straight-use-package 'helm-sage)
(straight-use-package 'helm-pass)
(straight-use-package 'slack)
(straight-use-package 'magit)
(straight-use-package 'sage-shell-mode)
(straight-use-package 'ob-sagemath)
(straight-use-package 'recentf)
(straight-use-package 'recentf-ext)
(straight-use-package 'adoc-mode)
(straight-use-package 'verilog-mode)
(straight-use-package 'auth-source-pass)
(straight-use-package 'pass)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'ac-octave)
(straight-use-package 'flyparens)
(straight-use-package 'auctex)
(straight-use-package 'auto-complete-auctex)
(straight-use-package 'auto-complete-sage)
(straight-use-package 'grammarly)
(straight-use-package 'company-math)
(straight-use-package 'flymake)
(straight-use-package 'whole-line-or-region)
(straight-use-package 'cheatsheet)
(straight-use-package 'git)
(straight-use-package 'helm-org)
(straight-use-package 'helm-c-moccur)
(straight-use-package 'helm-c-yasnippet)
(straight-use-package 'helm-bibtex)
(straight-use-package 'helm-bibtexkey)
(straight-use-package 'spacemacs-theme)


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



(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auth-source-debug t)
 '(auth-sources (quote ("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))
 '(comment-style (quote indent))
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(display-line-numbers t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(global-visual-line-mode t)
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(package-check-signature (quote allow-unsigned))
 '(package-selected-packages
   (quote
    (recentf-remove-sudo-tramp-prefix recentf-ext color-theme-sanityinc-tomorrow ob-sagemath helm-sage helm-pass auth-source-pass pass yasnippet-snippets zenburn-theme ac-octave helm flyparens leuven-theme slack auto-complete-auctex auto-complete-sage sage-shell-mode org-ac org-beautify-theme org-bullets adoc-mode grammarly org-plus-contrib company-math flymake yasnippet auctex org)))
 '(sage-shell:sage-executable
   "C:\\Users\\Eric\\AppData\\Local\\SageMath9_0\\runtime\\bin\\bash.exe")
 '(tramp-default-method "plink")
 '(verilog-auto-arg-format (quote single))
 '(window-divider-mode nil))


 










(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
;;(package-initialize)




;; associate .txt files with asciidoc mode
(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))


(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; map alt-o to switch between open windows
(global-set-key (kbd "M-o") 'other-window)

;; allow switching of windows with combo of shift and arrow key
(windmove-default-keybindings)


;; make pretty utf-8 style bullets in org mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; ;; Hunspell setup
;; (add-to-list 'exec-path "C:\msys64\mingw64\bin")
;; (setenv "LANG" "en_US, ru_RU")
;;   (setq-default  ispell-program-name "c:/msys64/mingw64/bin/hunspell.exe")
;;   (with-eval-after-load "ispell"
;;     (setq ispell-really-hunspell t)
;;     (setq ispell-program-name "hunspell")
;;     (setq ispell-dictionary "en_US")
;;     ;; ispell-set-spellchecker-params has to be called
;;     ;; before ispell-hunspell-add-multi-dic will work
;;     (ispell-set-spellchecker-params)
;;     (ispell-hunspell-add-multi-dic "en_US"))

;;        (require 'ispell)
;; ;; Hunspell setup end


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

(setq auth-source-debug t)


;(setq auth-sources '((:source "~/.authinfo.gpg")))



;(slack-register-team
; :name "nexgarden"
; :token (auth-source-pick-first-password
;	 :host "nexgarden.slack.com"
;	 :user "akier@pdx.edu")
; :subscribed-channels '((channel1 channel2)))
;     
(use-package helm-pass)



;; global shortcut key for magit-status command
(global-set-key (kbd "C-x g") 'magit-status)

;; allow info to visit links to 'gitman' info manual
(setq magit-view-git-manual-method 'man)



;; defining helm sage keyboard shortcuts
(eval-after-load "sage-shell-mode"
  '(sage-shell:define-keys sage-shell-mode-map
     "C-c C-i"  'helm-sage-complete
     "C-c C-h"  'helm-sage-describe-object-at-point
     "M-r"      'helm-sage-command-history
     "C-c o"    'helm-sage-output-history))






;; run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
(sage-shell:define-alias)

;; Turn on eldoc-mode in Sage terminal and in Sage source files
(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)


;; If you want to enable inline display of LaTeX outputs only,
;; uncomment the following line.
;;(setq sage-shell-view-default-commands 'output)

;; If you want to enable inline display of plots only,
;; uncomment the following line.
;; (setq sage-shell-view-default-commands 'plot)

(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)


;; sample ob-sagemath config

;; Ob-sagemath supports only evaluating with a session.
(setq org-babel-default-header-args:sage '((:session . t)
                                           (:results . "output")))



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
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 113 :width normal)))))

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
