;;;
;;; Ver 4.9 20171228 u1404 zot/rubr + vm
;;;
;;; 4.1    add various things from gh:marshroyer/emacs-dotfiles
;;; 4.2    move to init.el
;;;        add docs about bookmarking
;;;        add .emacs.d/.init.local.el
;;;        move .emacs_lib -> .emacs.d/lib
;;;        support for position with large displays and small.
;;; 4.3    update various org things, todo keywords
;;;        add gitflow
;;;        set magit-repository-directories
;;;        add ido-enter-magit-status to ido-setup-hook
;;;        add tramp, ido-remove-tramp-from-cache
;;; 4.4    add ediff customization
;;; 4.5    add show-trailing-whitespace and hooks
;;;        default face white on black
;;; 4.6    get c indenting to work again (nesc doesn't work anymore)
;;;        default face black on white.
;;; 4.7    nuke banish
;;; 4.8    add realgud
;;; 4.9    pdb mode hook to do no-trailing-whitespace
;;;
;;; x.x    add python Emacs IDE
;;;

;; (if nil
;; (cond
;;  ((file-newer-than-file-p "~/.emacs.el" "~/.emacs")
;;   (let ((mode-line-format "Recompiling .emacs..."))
;;     (message "Wait!  Your .emacs.el needs recompiling...")
;;     (sit-for 1)
;;     (byte-compile-file "~/.emacs.el")
;;     (rename-file "~/.emacs.elc" "~/.emacs" t)
;;     (message "Surfs up.  I'm outo here, Dude...")
;;     (sit-for 2)
;;     (kill-emacs)
;;     )))
;; )

(let ((local-settings "~/.emacs.d/.init.local.el"))
  (if (file-exists-p local-settings)
      (load-file local-settings)))

;;;
;;; frame positions dependent on what system we are actually displaying on
;;;

(when (display-graphic-p)
  (let ((remote_node
         (let ((node (getenv "SSH_CONNECTION")))
           (if (stringp node)
               (car (split-string node))
             "")))
        (rubr      "192.168.1.6" )
        (skoos-pro "192.168.1.93"))
    (cond
     ((and (string-equal system-name "zot")
           (string-equal remote_node ""))
        (message "zot, local")
        (setq initial-frame-alist
              '((top   .   0) (left   .    0)
                (width . 100) (height .   50)))
        (setq default-frame-alist
              '((top .     0) (left   . 1000)
                (width . 115) (height .   58)))
        (set-frame-size (selected-frame) 100 50))

     ((or (string-equal system-name "rubr.priv")
          (string-equal remote_node rubr))
        (message "rubr: %s %s" system-name rubr)
        (setq initial-frame-alist
              '((top   .   0) (left   . 1100)
                (width . 160) (height .   74)))
        (setq default-frame-alist
              '((top .     0) (left   . 1600)
                (width . 115) (height .   58)))
        (set-frame-size (selected-frame) 160 74))

     ((string-equal remote_node skoos-pro)
        (message "skoos-pro")
        (setq initial-frame-alist
              '((top   .   0) (left   .    0)
                (width . 100) (height .   50)))
        (setq default-frame-alist
              '((top .     0) (left   .  780)
                (width . 110) (height .   58)))
        (set-frame-size (selected-frame) 100 50)))))

;;;(set-frame-font "-adobe-courier-medium-r-normal--14-180-75-75-m-110-iso8859-1")

(setq load-path (append (list "~/.emacs.d/lib") load-path))
(load "misc")
(load "make")
(load "uniquify")
(setq uniquify-buffer-name-style 'forward)

(load "flushish")

(require 'package)
;; load packages ourselves
;; press C-h v on the variable for more documentation
(setq package-enable-at-startup nil)
;; Add MELPA to package-archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; load packages now
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Emacs adds `custom' settings in the init file by default. Run this file
;; without this segment to see what that means.
;; Put those away in "custom.el".
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :noerror)

;; update local database then install use-package if it's not installed
(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

(require 'use-package)
;; tell use-package to install a package if it's not already installed
(setq use-package-always-ensure t)


;;; (load-library "realgud")
;;; (setq realgud-safe-mode nil)

(require 'ido)
(ido-mode 1)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(add-hook 'ido-setup-hook
                    (lambda ()
                      (define-key ido-completion-map
                        (kbd "C-x g") 'ido-enter-magit-status)))

(defun ido-remove-tramp-from-cache nil
  "Remove any TRAMP entries from `ido-dir-file-cache'.
    This stops tramp from trying to connect to remote hosts on emacs startup,
    which can be very annoying."
  (interactive)
  (setq ido-dir-file-cache
        (cl-remove-if
         (lambda (x)
           (string-match "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\):" (car x)))
         ido-dir-file-cache)))

;; redefine `ido-kill-emacs-hook' so that cache is cleaned before being saved
(defun ido-kill-emacs-hook ()
  (ido-remove-tramp-from-cache)
  (ido-save-history))

;;;
;;; when emacs 25.1 comes along.
;;;
;;; (define-key ido-common-completion-map
;;;            (kbd "C-x g") 'ido-enter-magit-status)
;;;

;;;
;;; org mode
;;;
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time)
(setq org-agenda-files (list "~/.emacs.d/org/tag.org"
                             "~/.emacs.d/org/Misc.org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "FEEDBACK(f)" "VERIFY(v)" "|" "DONE(d)" "ABANDONED(a)" )))


;;; (fset 'yes-or-no-p 'y-or-n-p)

(setq display-time-interval 30)
(setq dired-dwim-target t)
(setq dired-backup-overwrite t)
(setq backward-delete-char-untabify-method 'all)
(display-time)
;;; (mouse-avoidance-mode 'banish)
(size-indication-mode t)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Allow repeated pops from the mark ring with C-u C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)

;;;
;;; grep stuff
;;;
(setq-default grep-template "grep <X> <C> -nHiR -e <R> <F>")
(setq grep-highlight-matches t)

;;;(grep-compute-defaults)
;;;(grep-apply-setting 'grep-command xxx)
;;;(eval-after-load "grep"
;;;  '(progn
;;;     (add-to-list 'grep-find-ignored-files "*.tmp")
;;;     (add-to-list 'grep-find-ignored-directories "_darcs")))
;;;add GREP_OPTIONS="--exclude=*#* --exclude=...

;;;
;;; Dired stuff
;;;
(add-hook 'dired-load-hook
          (function (lambda ()
                      (load "dired-x")
                      ;; Set global variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      )))


;;;
;;; Shell stuff
(add-hook 'shell-mode-hook
          (function (lambda ()
                      (local-unset-key "\C-c\C-m")
                      )))
(setq shell-popd-regexp "popd\\|p")
(setq shell-prompt-pattern "^[^#$%>:\n]*[#$%>:] *")
(setq shell-pushd-regexp "pushd\\|pd")
(add-to-list 'same-window-buffer-names "*shell*")
;;; (define-key text-mode-map "\es" nil)
;;; (define-key text-mode-map "\eS" nil)
;;; (define-key indented-text-mode-map "\es" nil)
;;; (define-key indented-text-mode-map "\eS" nil)
(global-set-key "\e\es" 'shell)

;;;
;;; gdb stuff
;;; make
;;; magit
;;;

(load "gdbish")

(add-hook 'gdb-mode-hook
          (function (lambda ()
                      (local-set-key (kbd "^C^L") 'gdb-redraw))))

(global-set-key (kbd "<f12>") 'gdb-redraw)
(global-set-key (kbd "C-X SPC") 'gud-break)
(global-set-key (kbd "C-C g")   'gdb)

(global-set-key "\^C\^M" 'make)
(global-set-key (kbd "C-X c") 'compile-with-same-commands)

(global-set-key (kbd "C-X g")   'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;; (require 'magit-gitflow)
;;; (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(setq magit-repository-directories
      '(("~/Development/repositories" . 1)
        ("~/Development/repositories/ttn" . 1)))

(setq magit-repolist-columns
      '(("Name"    25 magit-repolist-column-ident nil)
        ("Branch"  20 magit-repolist-column-branch nil)
        ("B<U"      3 magit-repolist-column-unpulled-from-upstream nil)
        ("B>U"      3 magit-repolist-column-unpushed-to-upstream nil)
        ("Flag"     4 magit-repolist-column-flag nil)))

;;;
;;; turn on winner mode
;;;

(when (fboundp 'winner-mode)
  (winner-mode 1))

;;;
;;; ediff customization
;;;
;;; see https://oremacs.com/2015/01/17/setting-up-ediff/
;;;

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)


;;;
;;; Bookmarks.
;;;
;;; C-x r m     set a bookmark
;;; C-x r l     list bookmarks
;;; C-x r b     jump to bookmark

;;;
;;; C/Nesc Editing
;;;
;;; (autoload 'nesc-mode "nesc.el")
;;;(add-to-list 'auto-mode-alist '("\\.nc\\'" . nesc-mode))
(add-to-list 'auto-mode-alist '("\\.nc\\'" . c-mode))

(global-set-key "\^Cb" 'c-begin-function)

;;
;; code formatting
;;
(c-add-style "mine"
             '("linux"
               (c-basic-offset . 2)
               (c-tab-always-indent t)
               (c-auto-align-backslashes t)
               (c-offsets-alist . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . +)
                                   (statement-block-intro . +)
                                   (block-open        . 0)))))
(setq c-default-style "mine")
(setq-default comment-column 40)

;;;(defun my-c-mode-hook ()
;;;  (c-set-style "mine"))

;;;(defun my-c-mode-hook ()
;;;  (c-set-style "mine")
;;;  (setq c-basic-offset 2
;;;        c-tab-always-indent t
;;;        c-auto-align-backslashes t
;;;        c-offsets-alist '((arglist-close     . c-lineup-arglist)
;;;                          (substatement-open . 0)
;;;                          (case-label        . +)
;;;                          (block-open        . 0)))
;;;  (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
;;;  (c-set-offset 'inline-open '+)
;;;  (c-set-offset 'block-open '+)
;;;  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
;;;  (c-set-offset 'case-label '+))       ; indent case labels by c-indent-level, too

;;;(add-hook 'c-mode-common-hook 'my-c-mode-hook)


;;;(defconst my-c-style
;;;    (c-hanging-braces-alist     . ((substatement-open after)
;;;                                   (brace-list-open)))
;;;    (c-hanging-colons-alist     . ((member-init-intro before)
;;;                                   (inher-intro)
;;;                                   (case-label after)
;;;                                   (label after)
;;;                                   (access-label after)))
;;;    (c-cleanup-list             . (scope-operator
;;;                                   empty-defun-braces
;;;                                   defun-close-semi))
;;;    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
;;;                                   (substatement-open . 0)
;;;                                   (case-label        . 4)
;;;                                   (block-open        . 0)
;;;                                   (knr-argdecl-intro . -)))
;;;    (c-echo-syntactic-information-p . t)
;;;    )

(setq c-report-syntactic-errors t)
(setq c-echo-syntactic-information-p t)

(setq tramp-default-method "ssh")

;;
;; key modifications to suit my tastes (do I have more than one?)
;;

;;; (setq indent-line-function 'indent-relative-maybe)

(global-set-key "\^C\^B" 'single-buffer-menu)
(global-set-key "\^C\^H" 'command-history-mode)
(global-set-key "\^C\^N" 'other-window)
(global-set-key "\^C\^P" 'prev-window)
(global-set-key "\^C." 'set-mark-command)
(global-set-key "\^C>" 'scroll-right-all)
(global-set-key "\^Cn"   'other-window)
(global-set-key "\^Cp"   'prev-window)

;; Add a keystroke for renaming a buffer
(global-set-key "\C-cr" 'rename-buffer)

(global-set-key "\^X\^B" 'buffer-menu)
;;; (global-set-key "\^X\^C" 'give-info-about-exit)
(global-set-key "\^X]"	 'next-page-top)
(global-set-key "\^Xl" 'goto-line)
(global-set-key "\^Z"  'scroll-down)
(global-set-key "\e\e=" 'what-line)

(global-set-key "\e?" 'apropos)
(global-set-key "\e]" 'forward-paragraph)

;;
;; mode line
;;
(setq-default mode-line-position
              '((-5 "%p") (size-indication-mode ("/" (-5 "%I")))
                (line-number-mode
                 (" %l" (column-number-mode ":%c")))
                "  "
                ))

(setq-default mode-line-format
  '("%e" mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    mode-line-buffer-identification
    " "
    mode-line-position
    (vc-mode vc-mode)
    "  "
    mode-line-misc-info
    " %b "
    mode-line-modified
    " "
    mode-line-modes
    mode-line-end-spaces))

(setq-default mode-line-buffer-identification
              '(#("%5f " 0 3
   (local-map
    (keymap
     (header-line keymap
                  (mouse-3 . mode-line-next-buffer)
                  (down-mouse-3 . ignore)
                  (mouse-1 . mode-line-previous-buffer)
                  (down-mouse-1 . ignore))
     (mode-line keymap
                (mouse-3 . mode-line-next-buffer)
                (mouse-1 . mode-line-previous-buffer)))
    mouse-face mode-line-highlight help-echo "Buffer name\nmouse-1: previous buffer\nmouse-3: next buffer" face mode-line-buffer-id))))


;;;
;;; backup settings
;;;
(setq make-backup-files t
      backup-by-copying t)
(setq backup-by-copying-when-linked t)
(setq dired-kept-versions 3)
(setq trim-versions-without-asking nil)
(setq version-control t
      kept-new-versions 8
      kept-old-versions 4
      delete-old-versions t)

;;;
;;; Disables
;;;
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled t)
(put 'downcase-region 'disabled t)

;;;
;;; whitespace
;;;
(require 'whitespace)
(setq-default show-trailing-whitespace t)

(defun no-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook  'no-trailing-whitespace)
(add-hook 'eww-mode-hook          'no-trailing-whitespace)
(add-hook 'ielm-mode-hook         'no-trailing-whitespace)
(add-hook 'gdb-mode-hook          'no-trailing-whitespace)
(add-hook 'pdb-mode-hook          'no-trailing-whitespace)
(add-hook 'help-mode-hook         'no-trailing-whitespace)
(add-hook 'Buffer-menu-mode-hook  'no-trailing-whitespace)
(add-hook 'calendar-mode-hook     'no-trailing-whitespace)
(add-hook 'shell-mode-hook        'no-trailing-whitespace)
(add-hook 'magit-popup-mode-hook  'no-trailing-whitespace)
(add-hook 'compilation-mode-hook  'no-trailing-whitespace)
(add-hook 'process-menu-mode-hook 'no-trailing-whitespace)

;;
;; other variables
;;

(setq split-width-threshold  nil)
(setq split-height-threshold nil)
(setq ctl-arrow t)
(setq-default indent-tabs-mode nil
              tab-width 8)
(setq-default fill-column 75)
(setq auto-save-interval 240
      delete-auto-save-files t)
(setq delete-exited-processes t)
;;; (setq inhibit-eol-conversion t)
(setq inhibit-quit nil)
(setq inhibit-startup-message t)
(setq inverse-video t)
(setq pop-up-windows t)
;;;(setq scroll-step 0)
(setq search-slow-window-lines 6)
(setq show-trailing-whitespace t)
(setq track-eol nil)
(setq truncate-lines nil)
(setq require-final-newline t)
(setq truncate-partial-width-windows nil)

(if (get-buffer "elisp")
    (progn (set-buffer "elisp")
           (lisp-interaction-mode)
           t)
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (rename-buffer "elisp")
  nil)

(setq-default major-mode 'indented-text-mode)
;;; (find-file-read-only "~/.ToDo")

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Ubuntu Mono")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-commit-arguments nil)
 '(magit-log-arguments (quote ("--graph" "--decorate" "-n256"))))

;; set default font size to 16pt
;;
(set-face-attribute 'default nil :height 160)

;; install magit
;;
(use-package magit
  :bind (("C-x g" . magit)))

;; install markdown viewer
;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; install markdown preview mode
;;
(use-package markdown-preview-mode)

;; Add Clojure support
;;
(add-to-list 'package-archives
                '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;; Add Graphviz support
;;

;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;(use-package company-graphviz-dot)

;; more setup from "Emacs for Clojure" book
;;
(use-package tagedit)
;;(use-package spinner)
(use-package smex)
(use-package sesman)
(use-package rainbow-delimiters)
(use-package queue)
(use-package projectile)
(use-package pkg-info)
(use-package parseedn)
(use-package parseclj)
(use-package paredit)
(use-package memoize)
(use-package magit-popup)
(use-package ghub)
(use-package exec-path-from-shell)
(use-package epl)
(use-package clojure-mode)
(use-package clojure-mode-extra-font-locking)
(use-package cider)
(use-package a)


;; set default font (fixed-width)
(cond
 ((string-equal system-type "darwin") ; macOS
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo-16" t t)))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Ubuntu Mono" (font-family-list))
    (set-frame-font "Ubuntu Mono-16" t t))))

;; Python Development Envirnoment
(use-package elpy
  :ensure t
  :init
  (elpy-enable))


;; When using this directly, you will need to have use-package installed:
;; M-x package-install, select use-package. But if you start via
;; `standalone.el', this is being taken care of automatically.


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Create / cleanup rust scratch projects quickly

(use-package rust-playground :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	   :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; GPT related software
(use-package gpt)
(require 'gpt)
;; USE ENV Variable for API key - DO NOT hardcode!
;; Set OPENAI_API_KEY in your environment
;;(setq gpt-openai-key (getenv "OPENAI_API_KEY"))
(setq gpt-openai-engine "gpt-4")
(setq gpt-openai-use-chat-api t)
;;(setq gpt-openai-org "personal")  ;; NOT SET
(setq gpt-openai-max-tokens 2000)
(setq gpt-openai-temperature 0)

;; org-ai
;; Set OPENAI_API_KEY environment variable instead of hardcoding
(setq org-ai-openai-api-token (getenv "OPENAI_API_KEY"))
(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

(use-package gptel
 :ensure t
 :config
 ;; Use environment variable for API key - DO NOT hardcode!
 (setq gptel-api-key (getenv "OPENAI_API_KEY")))

