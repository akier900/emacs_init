;;; 

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
;; ALso disable org prompt. 
(setq org-confirm-babel-evaluate nil)


(require 'package)

;; add Melpa repository
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
;; 
(package-initialize)


; myPackages contains a list of package names
(defvar myPackages
  '(ein					;jupyter notebok integration
    better-defaults
    )
  )


;; scans the list in myPackages
;; If not installed yet, install it
(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)



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
(straight-use-package 'auctex)
(straight-use-package 'auto-complete-auctex)
(straight-use-package 'auto-complete-sage)
(straight-use-package 'cc-mode)
(straight-use-package 'corral)
(straight-use-package 'company)
(straight-use-package 'company-c-headers)
(straight-use-package 'company-jedi)
(straight-use-package 'company-anaconda)
(straight-use-package 'company-math)
(straight-use-package 'dumb-jump)
(straight-use-package 'el-patch)
(straight-use-package 'elpy)
(straight-use-package 'flycheck)
(straight-use-package 'flyparens)
(straight-use-package 'ggtags)
(straight-use-package 'git)
(straight-use-package 'header2)
(straight-use-package 'helm)
(straight-use-package 'helm-bibtex)
(straight-use-package 'helm-bibtexkey)
(straight-use-package 'helm-c-moccur)
(straight-use-package 'helm-c-yasnippet)
(straight-use-package 'helm-org)
(straight-use-package 'helm-sage)
(straight-use-package 'magit)
(straight-use-package 'nyan-mode)
(straight-use-package 'org)
(straight-use-package 'org-bullets)
(straight-use-package 'recentf)
(straight-use-package 'recentf-ext)
(straight-use-package 'smartparens)
(straight-use-package 'sage-shell-mode)
(straight-use-package 'sr-speedbar)
(straight-use-package 'sublime-themes)
(straight-use-package 'use-package)
(straight-use-package 'verilog-mode)
(straight-use-package 'whole-line-or-region)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'py-autopep8)
(straight-use-package 'projectile)
(straight-use-package 'neotree)
(straight-use-package 'doom-themes)
(straight-use-package 'anaconda-mode)

;; NOTE				 
;; ein should be installed from Melpa
;; no other configuration is needed.


;; ========================
;; Development Setup
;; ========================
;; Enable elpy
(elpy-enable)


;; only leave uncommented if working in linux
;; Enable Flycheck
;;(when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8 autoformatter
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; ggtags setup
(require 'ggtags)
(add-hook 'c-mode-common-hook 'hs-minor-mode
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

;; When you press RET, the curly braces auto add a newline
(sp-with-modes '(c-mode c++-mode)
	       (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
	       (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
							 ("* ||\n[i]" "RET"))))
	     

;; map F5 to compile
;; (global-set-key (kbd "<f5>") (lambda ()	
;; 			       (interactive)
;; 			       (setq-local compilation-read-command nil)
;; 			       (call-interactively 'compile)))

;; Enable projectile mode and define default keymap
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)





;; require files for auto-completion "as you type"
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)



(defun my-python-mode-hook ()
  (progn
    (add-to-list 'company-backends 'company-anaconda)
    (add-to-list 'company-backends 'company-jedi)))
  

;; set company backends for working with python
(add-hook 'python-mode-hook 'my-python-mode-hook)





;; come back and add hooks and backends for the languages I work with


;; configure flycheck for sage-shell-mode and python-mode
;; Code taken from sage-shell-mode documentation 
;;(dolist (ckr '(python-pylint python-flake8))
;;  (flycheck-add-mode ckr 'sage-shell:sage-mode))

;;(defun sage-shell:flycheck-turn-on ()
;;  "Enable flycheck-mode only in file ended with py."
;;  (when (let ((bfn (buffer-file-name)))
;;	  (and bfn (string-match (rx ".py" eol) bfn)))
;;    (flycheck-mode 1)))

;;(add-hook 'python-mode-hook 'sage-shell:flycheck-turn-on)

(add-hook 'python-mode-hook 'anaconda-mode)




;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
	     "jupyter")

;;===========================================
;; Development setup end
;;===========================================




;; Enable dired-x for additional dired commands
(require 'dired-x)
(require 'sage-shell-mode)


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





;; Octave mode SETUP ===============================================

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


;; Octave mode SETUP end  ===============================================



;;  Magit setup ==================================
;; global shortcut key for magit-status command
(global-set-key (kbd "C-x g") 'magit-status)

;; allow info to visit links to 'gitman' info manual
(setq magit-view-git-manual-method 'man)
;;  Magit setup end  ==================================



;; Org Mode ===========================================
;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; Do not evaluate code blocks when exporting.
(setq org-export-babel-evaluate nil)

;; Show images when opening a file.
(setq org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; Org Mode ===========================================




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
 '(default ((t (:family "Fira Code" :foundry "outline" :slant normal :weight normal :height 109 :width normal))))
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
 '(custom-enabled-themes '(doom-molokai))
 '(custom-safe-themes
   '("0b9803c76a76f283dd3f2ac69a323a94373fb6170f4870d45b46c1e653275da7" "d77135b2056c1478e7f3cb43d544db445694ca62e4d5f2abee45472f8c8e30ba" "165e3571f414b5f18517431f1fe6d1c7b62a0dad26e781be02ff2b3b91d49f17" "427333ca345649edad1166c767bfb8511c2620720908695274a6b513b1f856d4" "1d75ba6aab722be7b13e72e159073bf2114997ef4f0f80442169e47974cfd753" "7b2f3f5064423615c6cb185afba6718f36750ee030b70386974b58c0aec744a2" "eadcac2381c964a1f92047a2de1c9fd05ab587e1114a8ef62eb5de0165493285" "8b1b05899b1614f9b7fdc249e557e2f5dbe098c0c5e5ac22e941ade3aa268428" "e08833c5dc1ba09647d6f7d2d55579fa25fe1b8c47038513007c41541667d9c8" "9df867652415c5a54d184fd5010b31b47f5dcbb260fdef410d226165b719a941" "25e94692dddd724c4296fafc2275f3a42716f9a2afa77db2bf1ca7c2f40fcb54" "e03d85fe9768013c3172a8773fb18335380ec56a5c4d3065b984788899e26be7" "20a9c07efbb18e71b9f2d25ffaad82e134e383c8aadb3f157ec0ae23eae5f34a" "78f9793674cc82b887bcd7df27c1c89ec0f9b89aaa51ed81bcb6f4ea49ef31a7" "5e57277a3ab7b6db80a79b8c6eca2aa3b6ffafd5a16745d343da0b9a1eddca1f" "1ce5477697ab3d44b0ab63dcace6212e93eebb0bece1ad81978b77b36b0fd93a" "ac912f2a75346acdcee4f769435596e0f213599d11e377f58e5c864cf05cd46b" "ae1e0db20067ca5998c1c3db7b5b507de493b9b22c0f63e0273cd7dd2f7c17aa" "be5aaca5649887462969b5175b638a7f0d852300eafb520604a06d15e0648e40" "20654ff745316269a65f18bff50475f23bfad91e7c133c782220b88df902e8b7" "fb7202718e15af960837e33eae24fa883b335725c78cc1363811ac3cb009c48a" "922978877d8f3ca57fe06c61d826161b30ab61cccc3b9ae0b1d862c2ee4b8158" "4b6e1a11563b7b44df8ffa8680168225527762f477dfda612f23ee02d114e33c" "df92121df7b20cf25452483cb6995818dda9df10058239ae86d4d8405393302a" "e95fe28e830d6b52d0336979b31922a088feb83cd148f1dcdc688c00ad72b1de" "b01f0f0b6ac5d17e4488163668ea35635a53557f205b5841a589fff4af6428ee" "ecb923cbeddeadab6b60cb64ab56e57a3a910c678480f947ae679a1b824f6de0" "2db16466d6e0374e0f220dd1b0dea1bde483fb792d5fbf5d27b7a828723b7560" "a72f2effb7752125193a6d7c5bc4be20b6966febaa4c347745bb47aaf7cac3c0" "8d83a3bd4f9ed5e6dc67d8ad8d7867dd2eb27a822b01fccbf8351f6808f764c6" "2bc02a7d0304b9777d03f4d27af5cba46b8a310250ba2602daadafd4c12a00b4" "b053b3ab4c1ee722b60bc109903131907e1b858ed5bfdf7af4a22056a45b95f5" "071cb4e4867144e6288e8cfe3678949359fa88c2b46224ca573858771c6afc9f" "3c3c3b9d4a336e82e9c1681f89bc3b97b937a272fd9b9db0bf7d2c3483e270a5" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "b267f390ae9919ae61fd6b9973971585ed71bc069a08196b136fd0389d4bc12b" "f6f566f0c8e76d5339bd9368b816a7e57f4fb318e9795033e47d60e8dc121bf2" "730277652b2e8eeb072604bc779a5782f7a4fbc0cf7803c69601b4be8a681d87" "dba1b403539040029374556514170fab030572b7a99d031cb0c29deca5872524" "8a881af89b6790a905bae2f11bb0b93010ebcd010bdc79104087aef77b22d8d7" "35ce30aa61c3d288dfb6f0687420d8773c6281e77cf07dc9dc9e9e9c315d29ae" "ff9df472cd58c2c226e8a11c36b2bc88e95eeb1666740ecb46e0155ce55073af" "03db8a813340989af5bd1bc24578d5e0cac295dcaaf30dc9546891bea0249900" "f2a626e8b41f12afbf3acc081dde9387b85b80525dbc70e9b61850a774c37e7a" "8da4938e0e5754d199ef23087edbddfadf78ecbacbd49b6c766d64296310e3e3" "9e009e887a64cffcb6e51946a63562ccbb3b177a8cd285571a5737757793baf5" "84c307eb4d445f8cff00eb315939652c8cfa7d1e08cc16861df8fdd2c07b66ff" default))
 '(display-line-numbers t)
 '(elpy-syntax-check-command "pylint")
 '(fci-rule-color "#858FA5")
 '(gas-comment-char 59)
 '(global-semantic-idle-completions-mode t nil (semantic/idle))
 '(global-semantic-idle-summary-mode t)
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#100E23" "#906CFF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100E23" "#95FFA4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100E23" "#565575"))
 '(linum-format " %7i ")
 '(nyan-mode t)
 '(objed-cursor-color "#FF8080")
 '(pdf-view-midnight-colors (cons "#CBE3E7" "#1E1C31"))
 '(rustic-ansi-faces
   ["#1E1C31" "#FF8080" "#95FFA4" "#FFE9AA" "#91DDFF" "#C991E1" "#AAFFE4" "#CBE3E7"])
 '(show-paren-mode t)
 '(user-full-name "Eric Aki, Email: Akier@pdx.edu")
 '(vc-annotate-background "#1E1C31")
 '(vc-annotate-color-map
   (list
    (cons 20 "#95FFA4")
    (cons 40 "#b8f7a6")
    (cons 60 "#dbf0a8")
    (cons 80 "#FFE9AA")
    (cons 100 "#ffd799")
    (cons 120 "#ffc488")
    (cons 140 "#FFB378")
    (cons 160 "#eda79b")
    (cons 180 "#db9cbd")
    (cons 200 "#C991E1")
    (cons 220 "#db8bc0")
    (cons 240 "#ed85a0")
    (cons 260 "#FF8080")
    (cons 280 "#d4757d")
    (cons 300 "#aa6a7a")
    (cons 320 "#805f77")
    (cons 340 "#858FA5")
    (cons 360 "#858FA5")))
 '(vc-annotate-very-old-color nil))

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





;;  Sage shell Mode ==============================
;; Turn on eldoc-mode in Sage terminal and in Sage source files
(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)


;; Run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
(sage-shell:define-alias)

(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)

;; set helm commands
(eval-after-load "sage-shell-mode"
  '(sage-shell:define-keys sage-shell-mode-map
     "C-c C-i"  'helm-sage-complete
     "C-c C-h"  'helm-sage-describe-object-at-point
     "M-r"      'helm-sage-command-history
     "C-c o"    'helm-sage-output-history))

;; add ac sources for sage-shell-mode
(add-hook 'sage-shell:sage-mode-hook 'ac-sage-setup)
(add-hook 'sage-shell-mode-hook 'ac-sage-setup)
;; allow tab completion at point
(eval-after-load "auto-complete-sage"
  '(setq sage-shell:completion-function 'completion-at-point))

;;  Sage shell Mode setup end ==============================









;; Activation for dumb-jump package
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)




;; Code to fix emacs erros that pop up in newest version
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








