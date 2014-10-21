;; Magic .emacs file, hacked together by gergely.erdelyi@gmail.com
;; Bits and pieces of this file come from all over the 'Net.

(require 'cl)

;; Get the installed packages ready to roll right away
(require 'package)
(package-initialize)

;; Set up package sources
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(defvar survival-kit
  '(auto-complete clojure-mode css-mode elpy exec-path-from-shell flycheck helm git-gutter
    helm-cmd-t js2-mode maxframe powerline solarized-theme)
  "A list of packages needed for this setup to work")

(defun survival-kit-is-complete-p ()
  (loop for p in survival-kit
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun survival-kit-install ()
  (interactive)
  (unless (survival-kit-is-complete-p)
    (message "The survival kit is not complete! Installing the missing bits ...")
    (package-refresh-contents)
    (dolist (p survival-kit)
      (when (not (package-installed-p p))
	(message "Installing %s" p)
	(package-install p)))))

;; Make sure all my favourite packages are installed
(survival-kit-install)

(add-to-list 'load-path (expand-file-name "~/.elisp"))

;; Pull in some env vars for the Mac GUI version of Emacs
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "HELM_DEFAULT_REPO")
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "PYTHONPATH"))

;; UTF-8 is great(tm)
(set-language-environment "utf-8")

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

(defun backward-symbol (&optional arg)
  "Move backward until encountering the beginning of a symbol.
   With argument, do this that many times."
  (interactive "p")
  (forward-symbol (- (or arg 1))))

;; Set Macish shortcuts for window and frame switching
(global-set-key (kbd "C-{") 'other-window)
(global-set-key (kbd "C-}") 'other-window-previous)
(global-set-key (kbd "C-`") 'other-frame)

(defun other-window-previous ()
  (interactive)
  (other-window -1))

(global-set-key '[M-left]  'backward-symbol)
(global-set-key '[M-right] 'forward-symbol)

;; Disable ~ backups, that's what git is for ;)
(setq make-backup-files nil)

;; Set a decently sized font based on the screen resolution
(defun set-frame-font (frame)
  (if window-system
      (cond
       ;; 27" Apple display
       ((> (x-display-mm-width) 700)
	(set-frame-parameter frame 'font "Inconsolata 20"))
       ((< (x-display-mm-width) 350)
	(set-frame-parameter frame 'font "Inconsolata 16"))
       (t (display-warning :warning "Can not set font size for this resolution automatically")))))

(defun reset-font ()
  (interactive)
  (set-frame-font nil))

;; Fontify the current and any future frames
(reset-font)
(push 'set-frame-font after-make-frame-functions)

;; Enable column numbers
(column-number-mode)

;; Show matching parentheses
(show-paren-mode)

;; y will suffice for yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set up C/C++ indentation
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

;; Indent JavaScript with spaces
(setq js2-mode-hook
  '(lambda () (progn
    (set-variable 'indent-tabs-mode nil))))

;; Turn Command key into a Control. Emacs Pinky Problem be gone!
(setq mac-command-modifier 'control)

;; Turn off the toolbar
(tool-bar-mode 0)

;; Turn off scrollbars
(scroll-bar-mode 0)

;; Turn off the bell
(setq ring-bell-function 'ignore)

;; Integrate with Mac clipboard
(setq x-select-enable-clipboard t)

;; F1 should find me anything
(setq helm-cmd-t-default-repo "~/work/")
(require 'helm-C-x-b)
(global-set-key [f1] 'helm-C-x-b)
(setq helm-cmd-t-default-repo (getenv "HELM_DEFAULT_REPO"))

;; Enable auto-complete globally
(require 'auto-complete)
(global-auto-complete-mode t)

(require 'git-gutter)
(global-git-gutter-mode t)
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-diff)
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x r") 'git-gutter:revert-hunk)

;;; Bind RET to py-newline-and-indent
(add-hook 'python-mode-hook '(lambda ()
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; My favourite default minor-modes for elpy
(setq elpy-default-minor-modes '(eldoc-mode
				 flycheck-mode
				 yas-minor-mode
				 auto-complete-mode))
(add-hook 'python-mode-hook
	  (lambda ()
	    (elpy-mode 1)
	    (elpy-clean-modeline)
	    (flycheck-select-checker 'python-pylint)
	    (setq indent-tabs-mode nil)))

;; Activate html-mode for HTML files
(setq auto-mode-alist (cons '("\\.html$" . html-mode) auto-mode-alist))

;; Make Cmd-Left/Right jump between tags
(add-hook 'html-mode-hook
	  (lambda ()
	    (define-key html-mode-map (kbd "<C-left>") 'sgml-skip-tag-backward)
	    (define-key html-mode-map (kbd "<C-right>") 'sgml-skip-tag-forward)))

;; Setup Emacs to run bash as its primary shell.
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setq explicit-shell-file-name shell-file-name)
(setenv "SHELL" shell-file-name)
(setq explicit-sh-args '("-login" "-i"))

;; Use Spotlight for locate
(setq locate-command "mdfind")

;; Set up bookmarks (to be used w/ C-x r {m|b|l})
(setq
 bookmark-default-file "~/.elisp/bookmarks"
 bookmark-save-flag 1)

;; Insert a standard date
(defun insert-date ()
  (interactive
   (insert (format-time-string "%a, %d %b %Y %T %z"))))

;; Erlang mode set up for the MacPorts version
(setq load-path (cons "/opt/local/lib/erlang/lib/tools-2.6.6.5/emacs/"
		      load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(autoload 'erlang-mode "erlang-start" "Erlang mode" t)
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

;; Set up Slime for Clozure
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime")
(setq inferior-lisp-program "/opt/local/bin/ccl64 -K utf-8")
;; (autoload 'slime "slime" "SLIME" t)
(add-to-list 'auto-mode-alist '("\\.lisp\\'" .
				(lambda ()
				  (require 'slime)
				  (setq slime-net-coding-system 'utf-8-unix)
				  (slime-setup  '(slime-repl slime-asdf slime-fancy slime-banner)))))

;; A wrapper over git grep to supply useful defaults w/o asking each time
(defun my-git-grep (command-args)
    (interactive
     (progn
       (vc-git-grep (grep-read-regexp)
		    "*"
		    (vc-git-root (file-name-directory (or load-file-name buffer-file-name)))))))

(global-set-key (kbd "C-<f1>")  'my-git-grep)

;; emacsclient is nice, serve it well
(require 'server)
(if (not (eq t (server-running-p server-name)))
    (server-start))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

;; Auto-save one global desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))
(if (not (getenv "NO_DESKTOP_SAVE_MODE"))
    (desktop-save-mode 1))

;; Tweak the standard theme to be a bit less intrusive
;;  - Change the flymake faces to underlines
;;  - Make the mode line lighter (same is used for anything's header)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:inherit mode-line :background "#e9e2cb" :foreground "#465a61" :box nil :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" default))))

;; Set better colours for powerline and enable it
;; This needs to be done after the main theme is enabled
(setq powerline-color1 "#657b83")
(setq powerline-color2 "#839496")
(set-face-attribute 'mode-line nil
                    :foreground "#fdf6e3"
                    :background "#859900"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)
(powerline-default-theme)

;; Bump up the main frame to take the full screen
(maximize-frame)
