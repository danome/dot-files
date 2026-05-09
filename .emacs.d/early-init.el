;;; early-init.el --- Fast Emacs startup -*- lexical-binding: t; -*-

;; Disable package.el at startup for faster load
(setq package-enable-at-startup nil)

;; Disable UI elements early
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set font, size, and resize policy before the first frame is shown.
;; Font must be here so char dimensions are stable when the frame is created,
;; preventing the startup jump caused by init.el's font load shifting the size.
(setq frame-resize-pixelwise t)
(setq default-frame-alist
      '((width . 110) (height . 40) (font . "Ubuntu Mono-16")))

;; No splash screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; Optimize garbage collection
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Reset after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ; 16MB
            (setq gc-cons-percentage 0.1)))

;; Prevent unwanted runtime compilation for native-comp
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name "eln-cache/" user-emacs-directory)))

(provide 'early-init)
;;; early-init.el ends here