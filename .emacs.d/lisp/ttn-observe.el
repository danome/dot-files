;;; ttn-observe.el --- TTN Network Observation for Emacs -*- lexical-binding: t; -*-

(require 'transient nil t)

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
  (with-temp-buffer
    (let ((exit (apply #'call-process ttn-bin nil t nil "observe" args)))
      (if (and (integerp exit) (= exit 0))
          (buffer-string)
        (format "Error calling `ttn observe %s` (exit=%s)." (car args) exit)))))

(defun ttn-observe--display (buffer-name title content)
  "Display CONTENT in buffer named BUFFER-NAME with TITLE."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ttn-observe-mode)
        (insert content)
        (goto-char (point-min))))
    (display-buffer buf)))

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
(transient-define-prefix ttn-menu ()
  "TTN Network Menu"
  ["Observation"
   ("s" "Status (Evidence Ladder)" ttn-status)
   ("h" "History (Audit Logs)"    ttn-history)
   ("c" "Content (Storage State)" ttn-content)]
  ["Infrastructure"
   ("i" "Infra Status"           ttn-infra-status)
   ("u" "Infra Up"               (lambda () (interactive) (compile (format "%s infra up --metrics" ttn-bin))))
   ("d" "Infra Down"             (lambda () (interactive) (compile (format "%s infra down" ttn-bin))))]
  ["Management"
   ("l" "List Identities"        (lambda () (interactive) (compile (format "%s identity list" ttn-bin))))
   ("v" "Services List"          (lambda () (interactive) (compile (format "%s services list" ttn-bin))))])

(provide 'ttn-observe)
