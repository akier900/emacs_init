;; mode specific settings and hooks


;;======================================================================
;; company
;;======================================================================

;;; Code:
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-box)
(add-hook 'after-init-hook 'company-box-mode)


(add-hook 'anaconda-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends) '(company-anaconda company-jedi))))

;(add-hook 'cc-mode-hook
;	  (lambda ()
;	    (set (make-local-variable 'company-backends) 'company-c-)))


(add-to-list 'company-backends #'company-tabnine)
;; trigger completion immediately
(setq company-idle-delay 0)
;; Number the candidtates (use M-1, M-2, et... to select correct completion)
(setq company-show-numbers t)
;; 



;; 
;; python mode setup
;;




(defun my-python-mode-hook ()
  (progn
    (elpy-enable)			;enable elpy
    (add-to-list 'company-backends 'company-anaconda)
    (add-to-list 'company-backends 'company-jedi)))

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; ;; Use IPython for REPL
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;; 	     "jupyter")


;; japanese teacher kana bindings
;; (defvar kana-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "v" #'kana-validate)
;;     (define-key map "s" #'kana-say-question)
;;     (define-key map "p" #'kana-previous)
;;     (define-key map "n" #'kana-next)
;;     (define-key map "t" #'kana-toggle-kana)
;;     (define-key map "r" #'kana-toggle-random)
;;     (define-key map "l" #'kana-loop-toggle)
;;     (define-key map "]" #'kana-loop-inc)
;;     (define-key map "[" #'kana-loop-dec)
;;     (define-key map "a" #'kana-first)
;;     (define-key map "j" #'kana-jump)
;;     (define-key map "q" #'kana-quit)
;;     (define-key map "d" #'kana-details)
;;     map)
;;   "Keymap for `kana-mode'.")


