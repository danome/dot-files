;;; init.el --- Modern Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A clean, modern Emacs configuration with Magit, Claude CLI integration,
;; and development tools.

;;; Code:

;; Add custom lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Package management setup
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless package--initialized (package-initialize))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; Keep customizations in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Core Emacs Settings

;; Server for emacsclient
(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

;; Better defaults
(setq-default
 indent-tabs-mode nil              ; Spaces instead of tabs
 tab-width 4                       ; Set tab width
 fill-column 80                    ; Line wrap column
 truncate-lines t                  ; Don't wrap lines
 require-final-newline t           ; Newline at end of file
 show-trailing-whitespace t        ; Highlight trailing whitespace
 ring-bell-function 'ignore        ; No bell
 auto-save-default nil             ; No auto-save files
 make-backup-files nil             ; No backup files
 create-lockfiles nil)             ; No lock files

;; UI improvements
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(column-number-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Better scrolling
(setq scroll-margin 3
      scroll-conservatively 100
      scroll-preserve-screen-position t)

;; Enable useful disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; Essential Packages

;; exec-path-from-shell (macOS PATH fix)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID"))
  (exec-path-from-shell-initialize))

;; Magit - Git integration
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq magit-repository-directories
        '(("~/Development/repositories" . 2)))
  (setq magit-repolist-columns
        '(("Name" 25 magit-repolist-column-ident nil)
          ("Branch" 12 magit-repolist-column-branch nil)
          ("L<" 3 magit-repolist-column-unpulled-from-upstream nil)
          (">R" 3 magit-repolist-column-unpushed-to-upstream nil)
          ("Path" 99 magit-repolist-column-path nil))))

;; Transient (required by Magit)
(use-package transient
  :config
  (setq transient-history-file
        (expand-file-name "transient/history.el" user-emacs-directory)))

;; Company - Completion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-show-numbers t))

;; Which-key - Show keybindings
(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))

;; Markdown mode
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; YAML mode
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; JSON mode
(use-package json-mode
  :mode "\\.json\\'")

;; Dockerfile mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;;; Programming Languages

;; Python
(use-package python
  :ensure nil
  :config
  (setq python-indent-offset 4))

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

;; TypeScript
(use-package typescript-mode
  :mode "\\.ts\\'")

;;; Claude CLI Integration
(when (locate-library "boon-claude")
  (require 'boon-claude)
  (global-set-key (kbd "C-c m m") #'boon/claude-menu)
  (global-set-key (kbd "C-c m a") #'boon/magit-ai-commit-message)
  (global-set-key (kbd "C-c m d") #'boon/claude-docs-for-buffer)
  (global-set-key (kbd "C-c m r") #'boon/claude-refactor-region))

;;; Org Mode
(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-log-done 'time
        org-agenda-files '("~/org/")
        org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)" 
                    "|" "DONE(d!)" "CANCELLED(c@)"))))

;;; Navigation and Search

;; IDO mode
(use-package ido
  :ensure nil
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always))

;; Better IDO with flx
(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1))

;; IDO vertical mode
(use-package ido-vertical-mode
  :after ido
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;;; Project Management
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ido))

;;; Dired enhancements
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t
        dired-listing-switches "-alh"))

;;; Theme
(use-package modus-themes
  :config
  (load-theme 'modus-operandi t))  ; Light theme, use modus-vivendi for dark

;;; Key Bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c d") 'duplicate-line)

;; Better window navigation
(windmove-default-keybindings)

;;; Custom Functions
(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column col)))

;; Load local configuration if it exists
(let ((local-config (expand-file-name "init.local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load local-config)))

(provide 'init)
;;; init.el ends here