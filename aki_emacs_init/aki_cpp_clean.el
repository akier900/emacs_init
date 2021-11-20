;;; aki_cpp_clean.el --- This is my init file which started as an environment for c/c++ but is growing into something more
;;;; BASICS (package independent) ============================================================
;;;Code:



;;(server-start)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))


;; change to home dirctory so that emacs doesnt launch from chocolatey shim location
(cd "~/")

;; default font
;;(set-face-attribute 'default nil :family "Fira Code Retina" :height 110)





;; refresh packages everytime we open emacs (hopefully)
(unless package-archive-contents
  (package-refresh-contents))



;; setup auto-revert behavior for all file buffers as well as dired buffers
(global-auto-revert-mode)


;; line numbers always!
(global-display-line-numbers-mode)

;; visual line mode always!
(global-visual-line-mode)

;; get rid of startup screen
(setq inhibit-splash-screen t)
(setq visible-bell t)


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



;;;; keybindings

(global-set-key (kbd "M-o") 'other-window)

;;; wrap highlighted regions in quotes
;;; or remove quotes around expression
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-\\") 'delete-pair)



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
(straight-use-package 'doom-themes)
(straight-use-package 'inkpot-theme)
(straight-use-package 'sublime-themes)

(load-theme 'doom-badger)




;; git, helm, hippie expand
(straight-use-package 'magit)
(straight-use-package 'git)
(straight-use-package 'helm)
(straight-use-package 'helm-company)
(straight-use-package 'hippie-exp-ext)
(straight-use-package 'hippie-namespace)
(straight-use-package 'flx-ido)
(straight-use-package 'gnu-elpa-keyring-update)

;; packages for programming
;(straight-use-package 'powershell)
;(straight-use-package 'gnuplot)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'yasnippet-classic-snippets)
(straight-use-package 'projectile)
;(straight-use-package 'org-projectile)
;(straight-use-package 'unicode-math-input)
;(straight-use-package 'spice-mode)
(straight-use-package 'paredit)		;for parentheses auto-matching

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
;(straight-use-package 'treemacs-all-the-icons)
;(straight-use-package 'parrot)

;; misc
(straight-use-package 'which-key)	;helps to keep track of keybindings.
(straight-use-package 'smartparens)	;smart parentheses
(straight-use-package 'smart-semicolon)
(straight-use-package 'tiny)
(straight-use-package 'flycheck)
(straight-use-package 'company-quickhelp)
(straight-use-package 'cpp-auto-include) ;auto add necessary c++ header files
(straight-use-package 'htmlize)
(straight-use-package 'highlight2clipboard)


;; lsp-mode packages (fuck irony-mode)
;(straight-use-package 'lsp-mode)
;(straight-use-package 'lsp-ui)
;(straight-use-package 'lsp-treemacs)
;(straight-use-package 'helm-lsp)
;(straight-use-package 'dap-mode)
;(straight-use-package 'ccls)

;; company-tabnine
;(straight-use-package 'company-tabnine)

;; Specific Helm mode packages
(straight-use-package 'helm-company)
;(straight-use-package 'helm-file-preview)
(straight-use-package 'helm-flycheck)
(straight-use-package 'helm-gtags)
;(straight-use-package 'helm-org)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-ag)
(straight-use-package 'helm-c-yasnippet)

;; org mode packages
;(straight-use-package 'ox-mediawiki)
;; (straight-use-package 'org)
;; (straight-use-package 'org-superstar)
;; (straight-use-package 'org-beautify-theme)
;; (straight-use-package 'org-anki)
;; (straight-use-package 'org-chef)
;; (straight-use-package 'org-dashboard)
;; (straight-use-package 'org-fancy-priorities)
;; (straight-use-package 'org-gcal)
;; (straight-use-package 'org-link-beautify)
;; (straight-use-package 'org-make-toc)
;; (straight-use-package 'org-onenote)
;; (straight-use-package 'org-pomodoro)
;; (straight-use-package 'org-recur)
;; (straight-use-package 'org-rich-yank)

;; misc
;;(straight-use-package 'gscholar-bibtex)
;;(straight-use-package 'live-py-mode)	;interactive python coding (live preview)
(straight-use-package 'function-args)
(straight-use-package 'csv)		;for dealing with comma-seperated value files
(straight-use-package 'material-theme)
(straight-use-package 'flyspell)

;; CEDET
(straight-use-package 'speedbar)


;; for python mode. Ive given up modularizing this file a long time ago.
;(straight-use-package 'elpy)
;(straight-use-package 'company-jedi)
;(straight-use-package 'company-anaconda)
;(straight-use-package 'anaconda-mode)



;;;; End of package installations



;; projectile setup
;(projectile-mode +1)
;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)







;; cedet setup
(semantic-mode 1)
(global-ede-mode 1)

(defun my-semantic-hook ()
  "Add imenu integration to semantic."
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)
(global-semantic-idle-scheduler-mode 1)
(semantic-add-system-include "/home/usr/include/")
(semantic-add-system-include "/usr/include/")
(semantic-add-system-include "/usr/include/c++/")





;; Enable elpy
;(elpy-enable)


;; enable helm globally
(helm-mode 1)








;; company backends for python
;(defun my-python-mode-hook ()
;  (add-to-list 'company-backends 'company-anaconda))
  
  

;(add-hook 'python-mode-hook 'my-python-mode-hook)
;(add-hook 'python-mode-hook 'anaconda-mode)
;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
 



;; recentf setup. Can now use C-x C-r to bring up list of last 25 opened buffers
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)



;;;; Packages setup
;; setting up hunspell for spell checking

;; point to path of hunspell dictionaries (Arch)
;; dictionary was manually downloaded and place at
;; ~/Library/Spelling.
;; (setenv
;;  "DICPATH"
;;  (concat (getenv "HOME") "/Library/Spelling"))


;; 					;(setq ispell-program-name "C://Hunspell//bin//hunspell.exe") ; Windows path
;; (setq ispell-program-name "/usr/bin/hunspell") ;Arch distro path
;; (setq ispell-local-dictionary "en_US")





;







;; enable which-key globally
(add-hook 'after-init-hook 'which-key-mode)

;; org-mode setup
;(load (aki-get-fullpath "org-babel-config")) ;external file



;; use theme (needs to be after themes are installed
;(load-theme 'org-beautify t)		;make org-mode pretty


;; gdb use many-windows mode by default
(setq
 gdb-many-windows t
 gdb-show-main t			;show file containing main at startup
 )



;; lsp-mode-setup/ dap-mode-setup
;;(require 'lsp-mode)

;; helm-lsp
;;(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

;; dap-mode
;(require 'ccls)
;(require 'dap-cpptools)

;; active hydra whenever program hits breakpoint
(add-hook 'dap-stopped-hook (lambda (arg)
				    (call-interactively #'dap-hydra)))
	 





(require 'flx-ido)
;(ido-mode 1)
(flx-ido-mode 1)
(setq ido-everywhere nil)
;; disable ido faces to see flx highlights
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;; for "modern" machines, optimize garbage collection for flx-ido
(setq gc-cons-threshold 20000000)


;; hdl/verilog setup (hdl_checker must be in path and is installed by pip)
;; (custom-set-variables
;;  '(lsp-vhdl-sever 'hdl-checker))
;; (add-hook 'verilog-mode-hook 'lsp)
;; (add-hook 'vhdl-mode-hook 'lsp)


;; c/c++ setup
;; hooks for lsp-mode
;;(add-hook 'c++-mode-hook  'lsp)
;;(add-hook 'c-mode-hook 'lsp)
;;(add-hook 'objc-mode-hook 'lsp)



;; flycheck setup
(global-flycheck-mode)



;; smart-semicolon for programming modes
(add-hook 'prog-mode-hook #'smart-semicolon-mode)



;;;; HELM SETUP start
(require 'helm)
(require 'helm-config)

; use helm for finding other buffers
; improve system default commands with helm counterparts
 (global-set-key (kbd "C-x b") 'helm-buffers-list)
 (global-set-key (kbd "C-x r b") 'helm-bookmarks)
 (global-set-key (kbd "M-x") 'helm-M-x)
 (global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; ;; helm-swoop
 (global-set-key (kbd "M-i") 'helm-swoop)

;; helm-company setup
(autoload 'helm-company "helm-company")
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))


;;helm file preview setup
;(require 'helm-file-preview)
;(helm-file-preview-mode 1)


;; helm-flycheck setup
(require 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; helm/gtags setup
(load (aki-get-fullpath "setup-helm-gtags"))			;change path for specific machine


;; helm-c-yasnippet setup
;; this should allow easier searching for snippets
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/straight/repos/yasnippet-classic-snippets/snippets/emacs-lisp-mode/")


;; function-args package setup (shows inline arguments hint for c/c++ function at point
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)


 ;; check for errors
;;  helm-org setup
;(add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
;(add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))


; better general company-mode settings
(setq company-show-numbers t)
(company-quickhelp-mode)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.2)		;Default is 0.2


;; PROJECTILE and helm-projectile setup
;;(load (aki-get-fullpath "setup-projectile"))
;;;; HELM SETUP end

;;;; COMPANY MODE SETUP start
;; Company mode enable
(add-hook 'after-init-hook 'global-company-mode)

;; company box mode
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)


;; keybindings for company mode
;; make it so we can navigate completion popups with C-n and C-p
;; instead of default M-n and M-p
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))




;; company-tabnine setup
;;(require 'company-tabnine)
;;(add-to-list 'company-backends #'company-tabnine)







(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
;; only valid for for arch on desktop
(add-to-list 'company-c-headers-path-system "/usr/include/c++/11.1.0/")
;only valid for windows on surface
;; (add-to-list 'company-c-headers-path-system  "C:\\ProgramData\\chocolatey\\lib\\mingw\\tools\\install\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\8.1.0\\include\\c++")
;; company mode setup end



;; nope, dont uncomment
;;(set 'company-c-headers-path-system "/usr/include/c++/11.1.0/")







;; all the icons
(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))








(defun code-compile ()
  (interactive)
  " this is just a simple function to compile c/c++ single source files
   or small projects with a custom command"
  (unless (or (file-exists-p "Makefile")
	      (file-exists-p "makefile"))
    (set (make-local-variable 'compile-command)
	 (let ((file (file-name-nondirectory buffer-file-name)))
	   (format "%s  %s -o  %s -pedantic -Wall -g -std=c++17 "
		 (if (equal "cpp" "cpp") "g++" "gcc")
		 file (file-name-sans-extension file))))
		 (compile compile-command)))


;; set function to f5 shortcut
(global-set-key [f5] 'code-compile)


 









(eval-after-load 'c++-mode
  '(define-key c++-mode-map [f9] 'dap-add-breakpoint))
  

;; disable python-not-found warnings 
(defvar treemacs-no-load-time-warnings t)



;; ;; octave mode stuffs
;; (setq auto-mode-alist
;;       (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; (add-hook 'octave-mode-hook
;; 	  (lambda ()
;; 	    (abbrev-mode 1)
;; 	    (auto-fill-mode 1)
;; 	    (if (eq window-system 'x)
;; 		(font-lock-mode 1))))


;;;




;; handy code for converting buffer to postscript then piping to ps2pdf
;; in order to create a pdf version of emacs buffer with syntax highlighting.

;; name of command is modi/pdf-print-buffer-with-faces


(require 'ps-print)
(when (executable-find "ps2pdf")
  (defun modi/pdf-print-buffer-with-faces (&optional filename)
    "Print file in the current buffer as pdf, including font, color, and
underline information.  This command works only if you are using a window system,
so it has a way to determine color values.

C-u COMMAND prompts user where to save the Postscript file (which is then
converted to PDF at the same location."
    (interactive (list (if current-prefix-arg
                           (ps-print-preprint 4)
                         (concat (file-name-sans-extension (buffer-file-name))
                                 ".ps"))))
    (ps-print-with-faces (point-min) (point-max) filename)
    (shell-command (concat "ps2pdf " filename))
    (delete-file filename)
    (message "Deleted %s" filename)
    (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf"))))





;;(setq path-to-ctags "/usr/local/bin/ctags")


;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (shell-command
;;    (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name))))



;; ;; use svls systemverilog language server
;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration
;; 	       '(verilog-mode . "verilog")))

;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
;; 		    :major-modes '(verilog-mode)
;; 		    :priority -1
;; ;		    :activation-fn (lsp-activate-on "verilog")
;; 		    :server-id 'svls))
							  







;;(require 'lsp-verilog)

;; (custom-set-variables
;;   '(lsp-clients-svlangserver-launchConfiguration "/tools/verilator -sv --lint-only -Wall")
;;   '(lsp-clients-svlangserver-formatCommand "/tools/verible-verilog-format"))

;; (add-hook 'verilog-mode-hook #'lsp-deferred)




;;; aki_cpp_clean.el ends here










