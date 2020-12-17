(require 'package)
(package-initialize)

(require 'ido)

;; More stuff
;; shamelessly borrowed from xah-emacs
(defun aki-get-fullpath (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's 
file location.

Example: If you have this line
(aki-get-fullpath \"../xyz.el\")
in the file at 
/home/mary/emacs/emacs_lib.el
then the value returned is
/home/mary/xyz.el
Regardless how or where emacs_lib.el is called. 

This solves two problems

1)If you have a file 'A', that calls 'load' on a file at 'B',
and 'B' calls 'load' on file 'C' using a relative path, then 
Emacs will complain about being unable to find C. This is because
'load' doesnt switch the current directory. 

To solve this, when your code only knows the relative path to another file, 'C', you can use the vaiable 'load-file-name' to get the current file's 
full path, then use that with the relative path to get a full path of the 
file you are interested in. 

2) To find the current file's full path, Emacs has 2 ways:
'load-file-name' and 'buffer-file-name'. If the file is loaded by 'load'
then 'load-file-name' works but 'buffer-file-name' doesn't. If the file
is called by 'eval-buffer',then 'load-file-name' is nil. You want to be
able to get the current file's full path regardless of the method used."

  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path)
  )

;;----------------------------------------------------------------------
;; load basic emacs settings

(load (aki-get-fullpath "aki_emacs_settings"))

;;----------------------------------------------------------------------


;;----------------------------------------------------------------------
;; load packages
;;----------------------------------------------------------------------



;;----------------------------------------------------------------------
;; load files
;;----------------------------------------------------------------------

(load (aki-get-fullpath "aki_emacs_settings_external_packages"))
(load (aki-get-fullpath "aki_emacs_keybinding_mode_specific"))
;(load (aki-get-fullpath "aki_emacs_misc"))

