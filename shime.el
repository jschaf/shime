;;; shime.el --- Superior Haskell Integration Mode for Emacs
;;
;; Copyright (c) 2010, Chris Done
;; All rights reserved. See below for license.
;;
;;; Commentary:
;;
;; A major mode for interacting with a Haskell inferior process.
;
;; * Currently only supports GHCi. Hugs might work but probably
;;   not.
;; * Not tested on OS X or Windows. Should work on both.
;;
;;; License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the
;; following conditions are met:
;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the
;;       following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the
;;       following disclaimer in the documentation and/or other
;;       materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL Chris Done BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
;; THE POSSIBILITY OF SUCH DAMAGE.
;;
;;; Code:

;; Customizable variables
(defvar shime-program "ghci"
  "The inferior Haskell program to use.")

(defvar shime-prompt-string "λ> "
  "External (actually exposed) prompt string.")

(defvar shime-custom-prompt-regex "^λ> \\(.*\\)"
  "Regex to match the exposed prompt string.")

(defvar shime-process-name "shime"
  "The name of the process.")

(defvar shime-buffer-name "*shime*"
  "The name of the created buffer.")

(defvar shime-strings-en
  '((shime-process-died . "The Shime process died. Restart it? ")
    (shime-program-not-found
     . (lambda (name)
         (concat "Unable to find Shime program \"" name
                 "\" in PATH, would you like to enter a new path? ")))
    (shime-program-new-path . "New Shime program path: ")
    (shime-could-not-start . "Shime could not start."))
  "English language strings.")

(defvar shime-lang-set shime-strings-en
  "Default language set.")

;; Constants. Shouldn't need to change these.
(defvar shime-inner-prompt-string "Shime>"
  "Internal prompt string.")

(defvar shime-prompt-regex "Shime>$"
  "Regex to match the internal prompt string.")

(defvar shime-process-buffer ""
  "Buffer of text received from the inferior Haskell process.")

(defvar shime-intermediate nil
  "Toggle to indicate that we've received intermediate data with no newline.")

(defvar shime-first-line t
  "Used to ignore the first internal prompt.")

(defvar shime-history '("")
  "Used to store the prompt history.")

(defvar shime-history-index 0
  "The current history cycle index.")

(defvar shime-history-length 0
  "Just an optimisation so that we don't have to do an O(n) operation.")

(defvar shime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'shime-key-ret)
    (define-key map (kbd "C-j") 'shime-key-ret)
    (define-key map (kbd "M-p") 'shime-key-prev)
    (define-key map (kbd "M-n") 'shime-key-next)
    map)
  "Shime mode map.")

(define-derived-mode shime-mode nil "Shime"
  (make-local-variable 'shime-mode)
  (setq shime-mode t))

(defun shime ()
  "Start Shime."
  (interactive)
  (if (executable-find shime-program)
      (with-current-buffer (shime-buffer)
        (shime-mode)
        (shime-start-process)
        (shime-mutable))
    (if (y-or-n-p (funcall (shime-string 'shime-program-not-found)
                           shime-program))
        (progn
          (setq shime-program
                (read-string (shime-string 'shime-program-new-path)
                             shime-program))
          (shime))
      (message (shime-string 'shime-could-not-start)))))

(defun shime-mode ()
  "Start Shime mode."
  (interactive)
  (with-current-buffer (shime-buffer)
    (kill-all-local-variables)
    (shime-immutable)
    (use-local-map shime-mode-map)
    (setq major-mode 'shime-mode)
    (setq mode-name "Shime")
    (run-mode-hooks 'shime-mode-hook)))

(defun shime-buffer ()
  "Get the shime buffer."
  (get-buffer-create shime-buffer-name))

(defun shime-start-process ()
  "Start the inferior Haskell process."
  (interactive)
  (setq shime-process-buffer "") ;; Reset the buffer
  (let ((process-connection-type nil)) ;; Use a pipe.
    (start-process shime-process-name nil (executable-find shime-program))
    (let ((process (get-process shime-process-name)))
      (set-process-filter process #'shime-process-filter)
      (set-process-sentinel process #'shime-process-sentinel)
      (setq shime-response-data nil)
      (shime-send-ghci-special (concat "set prompt \""
                                       shime-inner-prompt-string
                                       "\""))
      (setq shime-first-line t))))

(defun shime-process-filter (process input)
  "Process anything recieved from the inferior Haskell process."
  ;; Peel lines off from the buffer
  (let ((parts (split-string input "\r?\n")) (first t))
    (while (cdr parts)
      (when shime-intermediate
        (shime-delete-line)
        (setq shime-intermediate nil))
      (shime-handle-line (if first
                             (progn
                               (setq first nil)
                               (concat shime-process-buffer (car parts)))
                           (car parts)))
      (setq parts (cdr parts))
      (setq shime-process-buffer ""))
    (if (not (string-match shime-prompt-regex (concat shime-process-buffer input)))
        (progn (setq shime-intermediate t)
               (setq shime-process-buffer (concat shime-process-buffer (car parts)))
               (shime-echo (car parts)))
      (shime-check-prompt (concat shime-process-buffer input)))))

(defun shime-check-prompt (s)
  "Check for a prompt in the input buffer."
  (when (string-match shime-prompt-regex s)
    (when shime-first-line
      (setq shime-process-buffer "")
      (setq shime-first-line nil)
      (shime-delete-line)) ;; Kill the old prompt.
    (shime-prompt-trigger)))

(defun shime-handle-line (line)
  "Handle a line recieved from the inferior GHCi process."
  (if (string-match shime-prompt-regex line)
      (shime-prompt-trigger)
    (shime-echo (concat line "\n"))))

(defun shime-prompt-trigger ()
  "Trigger showing the prompt. This might be hooked later."
  (shime-prompt))

(defun shime-prompt ()
  "Show the Shime prompt."
  (shime-echo shime-prompt-string))

(defun shime-delete-line ()
  "Delete the current line in the buffer."
  (goto-char (point-max))
  (delete-region (line-beginning-position) (line-end-position)))

(defun shime-echo (str)
  "Echo a new entry in the Shime buffer."
  (with-current-buffer (shime-buffer)
    (goto-char (point-max))
    (insert str))) ;; Dumb way.

(defun shime-process-sentinel (process event)
  "Process any status change events (e.g. the process has quit)."
  (shime-immutable)
  (if (not (string= event ""))
      (when (y-or-n-p (shime-string 'shime-process-died))
        (shime-start-process)
        (shime-mutable))
    (shime-mutable)))

(defun shime-immutable ()
  "Make the buffer immutable."
  (with-current-buffer (shime-buffer)
    (setq buffer-read-only t)))

(defun shime-mutable ()
  "Make the buffer mutable."
  (with-current-buffer (shime-buffer)
    (setq buffer-read-only nil)))

(defun shime-send-expression (e)
  "Send a Haskell expression."
  (shime-send-string (concat e "\n")))

(defun shime-send-string (s)
  "Send an arbitrary string to the process."
  (process-send-string (get-process shime-process-name) s))

(defun shime-send-ghci-special (s)
  "Send a special GHCi command."
  (shime-send-expression (concat ":" s)))

(defun shime-string (n)
  "Look-up a string with the current language."
  (let ((entry (assoc n shime-lang-set)))
    (if entry
        (cdr entry)
      (error (concat
              "Unable to retrieve language entry for "
              (symbol-name n)
              " from language set.")))))

(defun shime-key-ret ()
  "Handle return event."
  (interactive)
  (with-current-buffer (shime-buffer)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      (when (string-match shime-custom-prompt-regex line)
        (let ((expr (match-string 1 line)))
          (add-to-list 'shime-history expr)
          (setq shime-history-length (1+ shime-history-length))
          (setq shime-history-index (1- shime-history-length))
          (shime-echo "\n")
          (shime-send-expression expr))))))

(defun shime-step-history (direction)
  "Update the history index."
  (setq shime-history-index
        (if (> shime-history-length 0)
            (mod (+ shime-history-index direction)
                 shime-history-length)
          0)))

(defun shime-key-prev ()
  "Go back in the history."
  (interactive)
  (when (> shime-history-length 0)
    (with-current-buffer (shime-buffer)
      (shime-step-history -1)
      (shime-clear-prompt)
      (insert (nth shime-history-index shime-history)))))

(defun shime-key-next ()
  "Go forward in the history."
  (interactive)
  (when (> shime-history-length 0)
    (with-current-buffer (shime-buffer)
      (shime-step-history 1)
      (shime-clear-prompt)
      (insert (nth shime-history-index shime-history)))))

(defun shime-clear-prompt ()
  "Cleat the prompt."
  (interactive)
  (shime-delete-line)
  (shime-prompt))

(provide 'shime)

;;; shime.el ends here
