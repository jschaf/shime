;; Copyright (c) 2010, Chris Done
;; All rights reserved.
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

(defvar shime-program "ghci")
(defvar shime-process-name "shime")
(defvar shime-buffer-name "*shime*")
(defvar shime-prompt-regex "^[^>]+> \\(.+\\)")

;; English language strings.
(defvar shime-strings-en
  '((shime-process-died . "The Shime process died. Restart it? ")))

;; Default language set.
(defvar shime-lang-set shime-strings-en)

;; Define the mode
(define-derived-mode shime-mode nil "Shime"
  (make-local-variable 'shime-mode)
  (setq shime-mode t))

;; Define the keymap
(setq shime-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'shime-key-ret)
    map))

;; Start Shime.
(defun shime ()
  (interactive)
  (with-current-buffer (shime-buffer)
    (shime-mode)
    (shime-start-process)
    (shime-mutable)))

;; Start the mode
(defun shime-mode ()
  (interactive)
  (with-current-buffer (shime-buffer)
    (kill-all-local-variables)
    (shime-immutable)
    (use-local-map shime-mode-map)
    (setq major-mode 'shime-mode)
    (setq mode-name "Shime")
    (run-mode-hooks 'shime-mode-hook)))

;; Get the shime buffer.
(defun shime-buffer ()
  (get-buffer-create shime-buffer-name))

;; Start the inferior Haskell process.
(defun shime-start-process ()
  (let ((process-connection-type nil)) ;; Use a pipe.
    (start-process shime-process-name nil (executable-find shime-program))
    (set-process-filter (get-process shime-process-name)
                        #'shime-process-filter)
    (set-process-sentinel (get-process shime-process-name)
                          #'shime-process-sentinel)))

;; Process anything recieved from the inferior Haskell process.
(defun shime-process-filter (process incoming)
  (if (shime-mutable-p)
      (shime-echo incoming)))

;; Echo a new entry in the Shime buffer.
(defun shime-echo (str)
  (with-current-buffer (shime-buffer)
    (goto-char (point-max))
    (insert str)))

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

;; Handle return event.
(defun shime-key-ret ()
  (interactive)
  (with-current-buffer (shime-buffer)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
    (when (string-match shime-prompt-regex line)
      (shime-echo "\n")
      (shime-send-expression (match-string 1 line))))))
