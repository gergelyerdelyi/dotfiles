;; Magic .emacs file, hacked together by gergely.erdelyi@gmail.com
;; Bits and pieces of this file come from all over the 'Net.

;; Time how long loading this file takes
(require 'cl) ; a rare necessary use of REQUIRE
(defvar *emacs-load-start* (current-time))

(add-to-list 'load-path (expand-file-name "~/.elisp"))

(defun backward-symbol (&optional arg) 
  "Move backward until encountering the beginning of a symbol. 
   With argument, do this that many times." 
  (interactive "p") 
  (forward-symbol (- (or arg 1)))) 

;; Crazy keyboard shortcut settings
(global-set-key (kbd "M-]") 'other-window)
(global-set-key (kbd "M-[") 'other-previous-window)

(global-set-key (kbd "C-{") 'previous-tab-or-buffer)
(global-set-key (kbd "C-}") 'next-tab-or-buffer)

(define-key osx-key-mode-map [home] 'beginning-of-line)
(define-key osx-key-mode-map [end] 'end-of-line)

(global-set-key '[M-left]  'backward-symbol)
(global-set-key '[M-right] 'forward-symbol)

;; Fix indentation, the way I like it
(setq c-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4)
		(setq c-default-style "bsd")
		(setq c-basic-offset 4))))
(setq c++-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4)
		(setq c-default-style "bsd")
		(setq c-basic-offset 4))))

;; Turn Command key into a Control. Emacs Pinky Problem be gone!
(setq mac-command-modifier 'control)

;; Turn off the toolbar
(tool-bar-mode 0)

;; Kill smart-ass frame positioning
(smart-frame-positioning-mode nil)
;; ...and repositioning
(setq fit-frame-inhibit-fitting-flag t)

;; Integrate with Mac clipboard
(setq x-select-enable-clipboard t)

;; Custom Anything source to find all files in my work directory 
(defun my-get-source-directory (path)
  (expand-file-name "~/work/"))

(defvar my-anything-c-source-file-search
  '((name . "File Search")
    (init . (lambda ()
              (setq anything-default-directory
                    default-directory)))
    (candidates . (lambda ()
                    (let ((args
                           (format "'%s' \\( -path \\*/.svn \\) -prune -o -iregex '.*%s.*' -print"
                                   (my-get-source-directory anything-default-directory)
                                   anything-pattern)))
		      (start-process-shell-command "file-search-process" nil
						   "find" args))))
    (type . file)
    (requires-pattern . 4)
    (delayed))
  "Source for searching matching files recursively.")

;; Activate anything .. or something
(require 'anything-config)

(defun my-anything ()
   (interactive)
   (anything-other-buffer
    '(anything-c-source-buffers
      anything-c-source-file-name-history
      anything-c-source-files-in-current-dir
      my-anything-c-source-file-search)
    " *my-anything*"))

(global-set-key [f1] 'my-anything)

;;; bind RET to py-newline-and-indent
(add-hook 'python-mode-hook '(lambda () 
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; Load up Pymacs and rope and pylint
(autoload 'pymacs-load "pymacs" "pymacs" t)
(add-hook 'python-mode-hook
	  (lambda ()
	    (pymacs-load "ropemacs" "rope-")
	    (load-file "~/.elisp/pylint.el"))
	  (when (load "flymake" t)
	    (defun flymake-pylint-init ()
	      (let* ((temp-file (flymake-init-create-temp-buffer-copy
				 'flymake-create-temp-inplace))
		     (local-file (file-relative-name
				  temp-file
				  (file-name-directory buffer-file-name))))
		(list "epylint" (list local-file))))
	    (add-to-list 'flymake-allowed-file-name-masks
			 '("\\.py\\'" flymake-pylint-init))))
(add-hook 'python-mode-hook
	  (lambda ()
	    (flymake-mode 1)
	    (setq indent-tabs-mode nil)))

;; Load the hacked diff view mode
(add-hook 'diff-mode-hook
	  (lambda ()
	    (require 'diff-mode-)))

;; Setup Emacs to run bash as its primary shell.
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setq explicit-shell-file-name shell-file-name)
(setenv "SHELL" shell-file-name)
(setq explicit-sh-args '("-login" "-i"))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Use Spotlight for locate
(setq locate-command "mdfind")

;; Print the time info
(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
				     (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
