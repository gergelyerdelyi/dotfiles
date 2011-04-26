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
(global-set-key (kbd "C-{") 'other-window)
(global-set-key (kbd "C-}") 'other-window-previous)

(defun other-window-previous ()
  (interactive)
  (other-window -1))

;; Make Home/End keys work properly
(pc-bindings-mode)

(global-set-key '[M-left]  'backward-symbol)
(global-set-key '[M-right] 'forward-symbol)

(global-set-key "\M- " 'hippie-expand)

;; Disable ~ backups, that's what git is for ;)
(setq make-backup-files nil)

;; Set a decently sized font
(set-default-font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")

;; Enable column numbers
(column-number-mode)

;; y will suffice for yes
(defalias 'yes-or-no-p 'y-or-n-p)

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
		(list "/opt/local/bin/epylint-2.6" (list local-file))))
	    (add-to-list 'flymake-allowed-file-name-masks
			 '("\\.py\\'" flymake-pylint-init))))
(add-hook 'python-mode-hook
	  (lambda ()
	    (flymake-mode 1)
	    (setq indent-tabs-mode nil)))

;; Activate html-mode for HTML files
(setq auto-mode-alist (cons '("\\.html$" . html-mode) auto-mode-alist))

;; Make Cmd-Left/Right jump between tags
(add-hook 'html-mode-hook
	  (lambda ()
	    (define-key html-mode-map (kbd "<C-left>") 'sgml-skip-tag-backward)
	    (define-key html-mode-map (kbd "<C-right>") 'sgml-skip-tag-forward)))

;; Load the hacked diff view mode
(add-hook 'diff-mode-hook
	  (lambda ()
	    (require 'diff-mode-)))

;; Activate the full-ack mode for ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
(global-set-key '[f7] 'ack)

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

;; Activate Autopair for nice automagic () pairs
;; (require 'autopair)
;; (autopair-global-mode t) ;; enable in all buffers

;; Set up bookmarks (to be used w/ C-x r {m|b|l})
(setq
 bookmark-default-file "~/.elisp/bookmarks"
 bookmark-save-flag 1)

;; Erlang mode set up for the MacPorts version
(setq load-path (cons "/opt/local/lib/erlang/lib/tools-2.6.6.2/emacs/"
		      load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; Print the time info
(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
				     (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
