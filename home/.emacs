;; Magic .emacs file, hacked together by gergely.erdelyi@gmail.com
;; Bits and pieces of this file come from all over the 'Net.

(require 'cl)

;; Get the installed packages ready to roll right away
(require 'package)
(package-initialize)

;; Set up package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "~/.elisp"))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")))

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
(global-set-key (kbd "C-<prior>") 'other-window)
(global-set-key (kbd "C-{") 'other-window)
(global-set-key (kbd "C-<next>") 'other-window-previous)
(global-set-key (kbd "C-}") 'other-window-previous)
(global-set-key (kbd "C-`") 'other-frame)

(defun other-window-previous ()
  (interactive)
  (other-window -1))

(global-set-key '[M-left]  'backward-symbol)
(global-set-key '[M-right] 'forward-symbol)

;; Disable ~ backups, that's what git is for ;)
(setq make-backup-files nil)

;; Enable column numbers
(column-number-mode)

;; Show matching parentheses
(show-paren-mode)

;; Use 4 spaces for indentation by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

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

;; Turn Command key into a Control. Emacs Pinky Problem be gone!
(setq mac-command-modifier 'control)

;; Turn on standard copy & paste shortcuts (Cmd-C/V/X)
(cua-mode 1)

;; Turn off the menu
(menu-bar-mode 0)

;; Turn off the toolbar
(tool-bar-mode 0)

;; Turn off scrollbars
(scroll-bar-mode 0)

;; Turn off the bell
(setq ring-bell-function 'ignore)

;; Integrate with Mac clipboard
(setq x-select-enable-clipboard t)

;; Use the whole screen
(toggle-frame-maximized)

(use-package flycheck)

(use-package auto-complete
  :config
  (global-auto-complete-mode t))

(use-package helm)

(defun my-helm (&optional arg)
  (interactive "p")
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (unless helm-source-ls-git
    (setq helm-source-ls-git
          (helm-ls-git-build-ls-git-source)))
  (cond ((helm-ls-git-root-dir)
         (setq my-helm-sources
               '(helm-source-buffers-list
                 helm-source-files-in-current-dir
                 helm-source-ls-git)))
        (t (setq my-helm-sources
                 '(helm-source-buffers-list
                   helm-source-files-in-current-dir))))
  (helm-set-local-variable 'helm-ls-git--current-branch (helm-ls-git--branch))
  (helm :sources my-helm-sources
        :ff-transformer-show-only-basename nil
        :buffer "*helm*"))

(use-package helm-ls-git
  :init
  (require 'helm-config)
  (require 'helm-for-files)
  (require 'helm-buffers)
  (global-set-key [Scroll_Lock] 'my-helm)
  (global-set-key [f13] 'my-helm)
  (global-set-key (kbd "C-p") 'my-helm))

(use-package neotree)

(use-package ag
  :config
  (global-set-key (kbd "C-<Scroll_Lock>") 'ag-project)
  (global-set-key (kbd "C-<f13>") 'ag-project)
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

(use-package css-mode)

(use-package pyenv-mode)

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (elpy-enable))

(use-package js2-mode
  :config
  (setq js2-mode-hook
        '(lambda () (progn
                      (set-variable 'indent-tabs-mode nil)))))

(use-package auto-font-size
  :ensure f
  :config
  ;; Set the correct font on all frames
  (push 'set-frame-font after-make-frame-functions)
  (add-hook 'window-size-change-functions 'set-frame-font))

(use-package solarized-theme)

(use-package powerline
  :config
  (powerline-default-theme)
  (setq solarized-high-contrast-mode-line t))

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-diff)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-x r") 'git-gutter:revert-hunk))

(use-package ripgrep)

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

;; Disable GUI dialogs for yes-no questions. Because
;; a) I don't like them
;; b) they jam up the whole Emacs process on Mac
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
;; y will suffice for yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; Load machine-specific, local init file
(setq local-dot-emacs (expand-file-name "~/.emacs.local"))
(when (file-readable-p local-dot-emacs)
  (load-file local-dot-emacs))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default))))
