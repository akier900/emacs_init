;; Set cursor color

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(set-cursor-color "DarkOrchid3")



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
(straight-use-package 'pass)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'flyparens)
(straight-use-package 'auctex)
(straight-use-package 'grammarly)
(straight-use-package 'company-math)
(straight-use-package 'flymake)
(straight-use-package 'whole-line-or-region)
(straight-use-package 'git)
(straight-use-package 'helm-org)
(straight-use-package 'helm-c-moccur)
(straight-use-package 'helm-c-yasnippet)
(straight-use-package 'helm-bibtex)
(straight-use-package 'helm-bibtexkey)
(straight-use-package 'helm-company)
(straight-use-package 'dash)
(straight-use-package 'dired-hacks)
(straight-use-package 'highlight-indent-guides)
(straight-use-package 'verilog-mode)
(straight-use-package 'company)

;; This is only needed once near the top of file. This sets up use of package 'use-package'
(eval-when-compile
  (require 'use-package))


;; Set helm front-end for company, activate company everywhere and
;; bind helm-company to "C-:"
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; helps combat screen tearing in exchange for reduced performance
;; speed
(setq redisplay-dont-pause t)



;; Bind M-/ to M-x hippie-expand instead of dabbrev
(global-set-key "\M- " 'hippie-expand)


;; display tooltips in echo area instead of seperate frame
(tooltip-mode -1)
(setq tooltip-use-echo-area t) 


(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



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

(global-display-line-numbers-mode)


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
 '(default ((t (:family "Fira Code Medium" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

;; Trigger an insertion of relevant text depending on mode of
;; new file in current buffer. system default
(add-hook 'find-file-hook 'auto-insert)

;; START TABS CONFIG
(setq custom-tab-width 4)

;; Two functions for enabling/disabling in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))

(defun enable-tabs ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to enable tabs and indent guides
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'prog-mode-hook 'enable-tabs)

;; Hooks to disable
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make backspace properly erase tab instead of 1 space at a time
(setq backward-delete-char-untabify-method 'hungry)


;; END TABS CONFIG



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
 '(company-backends
   (quote
    (company-keywords company-bbdb company-semantic company-capf company-clang company-files
                      (company-dabbrev-code company-gtags company-etags company-cmake)
                      company-oddmuse company-dabbrev)))
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("ab2cbf30ab758c5e936b527377d543ce4927001742f79519b62c45ba9dd9f55e" default)))
 '(global-display-line-numbers-mode t)
 '(highlight-indent-guides-method (quote bitmap))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(inhibit-startup-screen t)
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(show-paren-mode t))

;; when 'M-x check-parens' is run, it will highlight ungrouped expression from
;; where your cursor cucrrently is
(show-paren-mode t)
(setq show-paren-style 'expression)
