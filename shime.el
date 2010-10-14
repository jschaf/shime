(defvar shime-program "/home/chris/Programs/bin/ghci")
(defvar shime-process-name "shime")
(defvar shime-buffer-name "*shime*")
(defvar shime-welcome-message "\"Hello, Haskell!\"")

;; English language strings.
(defvar shime-strings-en
  '((shime-process-died . "The SHIME process died. Restart it? ")))

;; Default language set.
(defvar shime-lang-set shime-strings-en)

;; Define the mode
(define-derived-mode shime-mode nil "SHIME"
  (make-local-variable 'shime-mode)
  (setq shime-mode t))

;; Define the keymap
(defvar shime-mode-map (make-sparse-keymap))

;; Start SHIME.
(defun shime ()
  (interactive)
  (with-current-buffer (shime-buffer)
    (shime-mode)
    (shime-start-process)
    (shime-mutable)))

;; Start the mode
(defun shime-mode ()
  (interactive)
  (kill-all-local-variables)
  (shime-immutable)
  (use-local-map shime-mode-map)
  (setq major-mode 'shime-mode)
  (setq mode-name "SHIME")
  (run-mode-hooks 'shime-mode-hook))

;; Get the shime buffer.
(defun shime-buffer ()
  (get-buffer-create shime-buffer-name))

;; Start the inferior Haskell process.
(defun shime-start-process ()
  (let ((process-connection-type nil)) ;; Use a pipe.
    (shime-buffer)
    (start-process shime-process-name nil shime-program)
    (set-process-filter (get-process shime-process-name)
                        #'shime-process-filter)
    (set-process-sentinel (get-process shime-process-name)
                          #'shime-process-sentinel)
    (shime-send-expression shime-welcome-message)))

;; Process anything recieved from the inferior Haskell process.
(defun shime-process-filter (process incoming)
  (message incoming))

;; Process any status change events (e.g. the process has quit).
(defun shime-process-sentinel (process event)
  (shime-immutable)
  (if (not (string= event ""))
      (when (y-or-n-p (shime-string 'shime-process-died))
        (shime-start-process)
        (shime-mutable))
    (shime-mutable)))

;; Make the buffer immutable.
(defun shime-immutable () 
  (with-current-buffer (shime-buffer)
   (setq buffer-read-only t)))

;; Make the buffer mutable.
(defun shime-mutable ()
  (with-current-buffer (shime-buffer)
    (setq buffer-read-only nil)))

;; Send a Haskell expression.
(defun shime-send-expression (e)
  (shime-send-string (concat e "\n")))

;; Send an arbitrary string to the process.
(defun shime-send-string (s)
  (process-send-string (get-process shime-process-name) s))

;; Look-up a string with the current language.
(defun shime-string (n)
  (let ((entry (assoc n shime-lang-set)))
    (if entry
        (cdr entry)
      (error (concat
              "Unable to retrieve language entry for "
              (symbol-name n)
              " from language set.")))))
