;;; ttn-observe.el --- TTN Network Observation for Emacs -*- lexical-binding: t; -*-

;; Version: 0.1.0 (based on ttn-docs 0.1.0)
;; URL: https://github.com/danome/dot-files
;; Keywords: tools, network

;;; Commentary:
;; This file provides Emacs integration for the ttn binary.
;; It loosely refers to ttn-docs for version tracking purposes.

(defgroup ttn-observe nil
  "TTN Network Observation options."
  :group 'tools)

(defcustom ttn-bin "ttn"
  "Path to the ttn binary."
  :type 'string
  :group 'ttn-observe)

(defvar ttn-observe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'ttn-observe-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "t") #'ttn-menu)
    map)
  "Keymap for `ttn-observe-mode'.")

(define-derived-mode ttn-observe-mode special-mode "TTN-Observe"
  "Major mode for displaying TTN network observation data."
  (setq-local revert-buffer-function #'ttn-observe-refresh))

(defun ttn-observe--call (args)
  "Call ttn binary with ARGS and return output."
  (let ((ttn-exec (or (executable-find ttn-bin)
                      (and (file-exists-p ttn-bin) ttn-bin))))
    (if (not ttn-exec)
        (format "TTN binary not found: %s" ttn-bin)
      (with-temp-buffer
        (condition-case err
            (let ((exit (apply #'call-process ttn-exec nil t nil "observe" args)))
              (if (and (integerp exit) (= exit 0))
                  (buffer-string)
                (format "Error calling `ttn observe %s` (exit=%s):\n\n%s"
                        (car args)
                        exit
                        (string-trim (buffer-string)))))
          (error
           (format "Error calling `ttn observe %s`:\n\n%s"
                   (car args)
                   (error-message-string err))))))))

(defun ttn-observe--display (buffer-name title content)
  "Display CONTENT in buffer named BUFFER-NAME with TITLE."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ttn-observe-mode)
        (insert content)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;;;###autoload
(defun ttn-status ()
  "Display TTN network status (Evidence Ladder)."
  (interactive)
  (let ((output (ttn-observe--call '("status"))))
    (ttn-observe--display "*TTN Status*" "Status" output)))

;;;###autoload
(defun ttn-history (&optional since)
  "Display TTN audit history. SINCE defaults to 7d."
  (interactive "sSince (e.g. 24h, 7d): ")
  (let* ((since (if (or (not since) (string-empty-p since)) "7d" since))
         (output (ttn-observe--call (list "history" "--since" since))))
    (ttn-observe--display "*TTN History*" "History" output)))

;;;###autoload
(defun ttn-content ()
  "Display TTN content and storage state."
  (interactive)
  (let ((output (ttn-observe--call '("content"))))
    (ttn-observe--display "*TTN Content*" "Content" output)))

;;;###autoload
(defun ttn-infra-status ()
  "Display TTN infrastructure status."
  (interactive)
  (with-temp-buffer
    (let ((exit (call-process ttn-bin nil t nil "infra" "status")))
      (let ((output (if (and (integerp exit) (= exit 0))
                        (buffer-string)
                      (format "Error calling `ttn infra status` (exit=%s)." exit))))
        (ttn-observe--display "*TTN Infra Status*" "Infra Status" output)))))

(defun ttn-observe-refresh ()
  "Refresh the current TTN observation buffer."
  (interactive)
  (cond
   ((string= (buffer-name) "*TTN Status*") (ttn-status))
   ((string= (buffer-name) "*TTN History*") (ttn-history))
   ((string= (buffer-name) "*TTN Content*") (ttn-content))
   ((string= (buffer-name) "*TTN Infra Status*") (ttn-infra-status))
   (t (message "Not in a TTN observation buffer"))))

;;;###autoload
(defun ttn-menu ()
  "Dispatch a TTN action without using transient side windows."
  (interactive)
  (pcase (read-char-choice
          "TTN: [s]tatus [h]istory [c]ontent [i]nfra [u]p [d]own [l]ist-identities ser[v]ices "
          '(?s ?h ?c ?i ?u ?d ?l ?v))
    (?s (call-interactively #'ttn-status))
    (?h (call-interactively #'ttn-history))
    (?c (call-interactively #'ttn-content))
    (?i (call-interactively #'ttn-infra-status))
    (?u (call-interactively #'ttn-infra-up))
    (?d (call-interactively #'ttn-infra-down))
    (?l (call-interactively #'ttn-identity-list))
    (?v (call-interactively #'ttn-services-list))))

(defun ttn-infra-up ()
  "Run `ttn infra up --metrics'."
  (interactive)
  (compile (format "%s infra up --metrics" ttn-bin)))

(defun ttn-infra-down ()
  "Run `ttn infra down'."
  (interactive)
  (compile (format "%s infra down" ttn-bin)))

(defun ttn-identity-list ()
  "Run `ttn identity list'."
  (interactive)
  (compile (format "%s identity list" ttn-bin)))

(defun ttn-services-list ()
  "Run `ttn services list'."
  (interactive)
  (compile (format "%s services list" ttn-bin)))

(provide 'ttn-observe)
