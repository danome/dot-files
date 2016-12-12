;;;
;;; Ver 4.0 20161016 u1404 zot/rubr + vm
;;;

(cond
 ((file-newer-than-file-p "~/.emacs.el" "~/.emacs")
  (let ((mode-line-format "Recompiling .emacs..."))
    (message "Wait!  Your .emacs.el needs recompiling...")
    (sit-for 1)
    (byte-compile-file "~/.emacs.el")
    (rename-file "~/.emacs.elc" "~/.emacs" t)
    (message "Surfs up.  I'm outo here, Dude...")
    (sit-for 2)
    (kill-emacs)
    )))

(if (string-equal system-name "rubr.priv")
    (setq initial-frame-alist
	  '((top . 0)
	    (left . 0)
	    (width . 160)
	    (height . 80))))

;;;(set-frame-font "-adobe-courier-medium-r-normal--14-180-75-75-m-110-iso8859-1")

(setq load-path (append (list "~/.emacs_lib") load-path))
(load "misc")
(load "make")
(load "uniquify")
(setq uniquify-buffer-name-style 'forward)

(setq display-time-interval 30)
(setq dired-dwim-target t)
(setq dired-backup-overwrite t)
(setq backward-delete-char-untabify-method 'all)
(display-time)
(mouse-avoidance-mode 'banish)
(size-indication-mode t)
(show-paren-mode t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(setq-default grep-template "grep <X> <C> -nHiR -e <R> <F>")
(setq grep-highlight-matches t)
;;;(grep-compute-defaults)
;;;(grep-apply-setting 'grep-command xxx)
;;;(eval-after-load "grep"
;;;  '(progn
;;;     (add-to-list 'grep-find-ignored-files "*.tmp")
;;;     (add-to-list 'grep-find-ignored-directories "_darcs")))
;;;add GREP_OPTIONS="--exclude=*#* --exclude=...

;;
;; hooks
;;

(add-hook 'dired-load-hook
          (function (lambda ()
                      (load "dired-x")
                      ;; Set global variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      )))

(add-hook 'shell-mode-hook
	  (function (lambda ()
		      (local-unset-key "\C-c\C-m")
		      )))

(autoload 'nesc-mode "nesc.el")
(add-to-list 'auto-mode-alist '("\\.nc\\'" . nesc-mode))
;;;(add-to-list 'auto-mode-alist '("\\.nc\\'" . c-mode))

;;
;; key modifications to suit my tastes (do I have more than one?)
;;

;;; (define-key text-mode-map "\es" nil)
;;; (define-key text-mode-map "\eS" nil)
;;; (define-key indented-text-mode-map "\es" nil)
;;; (define-key indented-text-mode-map "\eS" nil)
;;; (setq indent-line-function 'indent-relative-maybe)

(global-set-key "\^C\^B" 'single-buffer-menu)
(global-set-key "\^C\^H" 'command-history-mode)
(global-set-key "\^C\^M" 'make)
(global-set-key "\^C\^N" 'other-window)
(global-set-key "\^C\^O" 'co-file)
(global-set-key "\^C\^P" 'prev-window)
(global-set-key "\^C." 'set-mark-command)
(global-set-key "\^C>" 'scroll-right-all)
(global-set-key "\^Cb" 'c-begin-function)
(global-set-key "\^Cn"   'other-window)
(global-set-key "\^Cp"   'prev-window)
(global-set-key "\^X\^B" 'buffer-menu)
;;; (global-set-key "\^X\^C" 'give-info-about-exit)
(global-set-key "\^X]"	 'next-page-top)
(global-set-key "\^Xc"   'compile-with-same-commands)
(global-set-key "\^Xl" 'goto-line)
(global-set-key "\^Z"  'scroll-down)
(global-set-key "\e\e=" 'what-line)
(global-set-key "\e\es" 'shell)

(global-set-key "\e?" 'apropos)
(global-set-key "\e]" 'forward-paragraph)

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
                                   (block-open        . 0)))))
(setq c-default-style "mine")

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


;;
;; other variables
;;

(setq backup-by-copying-when-linked t)
(setq ctl-arrow t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(setq-default fill-column 75)

(setq delete-auto-save-files nil)
(setq delete-exited-processes t)
(setq delete-old-versions nil)
(setq dired-kept-versions 3)
;;; (setq inhibit-eol-conversion t)
(setq inhibit-quit nil)
(setq inhibit-startup-message t)
(setq inverse-video t)
(setq kept-new-versions 10)
(setq kept-old-versions 10)
(setq pop-up-windows t)
;;;(setq scroll-step 0)
(setq search-slow-window-lines 6)
(setq shell-popd-regexp "popd\\|p")
(setq shell-prompt-pattern "^[^#$%>:\n]*[#$%>:] *")
(setq shell-pushd-regexp "pushd\\|pd")
(setq track-eol nil)
(setq trim-versions-without-asking nil)
(setq truncate-lines nil)
;;;(setq truncate-lines t)
(setq truncate-partial-width-windows nil)
(setq version-control t)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled t)
(put 'downcase-region 'disabled t)

(if (get-buffer "elisp")
    (progn (set-buffer "elisp")
	   (lisp-interaction-mode)
	   t)
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (rename-buffer "elisp")
  nil)

(setq-default major-mode 'indented-text-mode)
(find-file-read-only "~/.ToDo")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(default ((t (:weight normal :height 120 :width normal :foundry "unknown" :family "Ubuntu Mono" :slant normal)))))
'(default ((t (:weight normal :invert t :height 120 :width normal :foundry "unknown" :family "Ubuntu Mono" :slant normal)))))
