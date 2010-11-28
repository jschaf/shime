;;; shime.el --- Superior Haskell Integration Mode for Emacs
;;
;; Copyright (c) 2010, Chris Done
;; All rights reserved. See below for license.
;;
;;; Commentary:
;;
;; A major mode for interacting with a Haskell inferior process.
;;
;; * Currently only supports GHCi.
;; * Not tested on OS X or Windows.  Should work on both.
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
;; Installation
;;
;; - Place `shime.el' in your emacs load path.
;; - Add the following line to your .emacs file:
;;   (autoload 'shime "shime" nil t)
;;
;; To customize shime:
;;  M-x customize-group RET shime RET

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'format-spec)

(defvar shime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'shime-key-ret)
    (define-key map (kbd "TAB") 'shime-key-tab)
    (define-key map (kbd "DEL") 'shime-key-del)
    (define-key map (kbd "C-j") 'shime-key-ret)
    (define-key map (kbd "M-p") 'shime-key-history-prev)
    (define-key map (kbd "M-n") 'shime-key-history-next)
    (define-key map (kbd "C-a") 'shime-key-home)
    map)
  "Shime mode map.")

(define-derived-mode shime-mode nil "Shime"
  (make-local-variable 'shime-mode)
  (setq shime-mode t))

(add-hook 'shime-mode-hook 'shime-set-cabal-commands)

;; Customization

(defcustom shime-default-ghci-path "ghci"
  "Default GHCi path."
  :group 'shime
  :type 'string)

(defcustom shime-default-shell-path "bash"
  "Default Cabal path."
  :group 'shime
  :type 'string)

(defcustom shime-cabal-program-path "cabal"
  "Default Cabal path."
  :group 'shime
  :type 'string)

(defcustom shime-default-language "en"
  "Default language."
  :group 'shime
  :type 'string)

(defcustom shime-default-session-name "shime"
  "Default session name."
  :group 'shime
  :type 'string)

(defcustom shime-ghci-prompt-regex "^λ> "
  "Regex to match the prompt string."
  :group 'shime
  :type 'string)

(defcustom shime-collapse-errors nil
  "Collapse GHCi errors onto a single line."
  :group 'shime
  :type 'boolean)

;; Constants

(defvar shime-strings-en "English language strings.")
(setq shime-strings-en
      `((process-died . "The Shime process died. Restart it? ")
        (program-not-found
         . (lambda (name)
             (concat "Unable to find Shime program \"" name
                     "\", what's the right path? ")))
        (could-not-start . "Shime could not start.")
        (enter-session-name . "Session name: ")
        (kill-session . "Kill Shime session: ")
        (kill-process . "Kill Shime process: ")
        (kill-buffer . "Kill Shime buffer: ")
        (start-shime . "Start Shime? ")
        (ghci-died-restart?
         . (lambda (name)
             (concat "The GHCi process \"" name "\" ended. Restart it? ")))
        (restarting-ghci-process . "Restarting GHCi process...")
        (buffer-no-processes . "No processes attached to this buffer!")
        (choose-buffer-process . "Choose buffer process: ")
        (ask-change-root . "Do you want to change the root directory? ")
        (new-load-root . "New load root: ")
        (new-cabal-root . "New Cabal root: ")
        (choose-session . "Choose session: ")
        (cabal-command-finished . "Cabal command finished.")
        (choose-buffer-ghci-process . "Choose GHCi process: ")
        (needed-a-session . "The command needed a Shime session. Aborted.")
        (buffer-session-was-set
         . (lambda (session-name)
             (concat "Buffer session set to: " session-name)))
        (buffer-session-was-set-default
         . (lambda (session-name)
             (concat "Buffer session set to (default): " session-name
                     " (Use `M-x shime-choose-buffer-session` to change.)")))
        (buffer-ghci-process-was-set
         . (lambda (process-name)
             (concat "Buffer GHCi process set to: " process-name)))
        (buffer-ghci-process-was-set-default
         . (lambda (process-name)
             (concat "Buffer GHCi process set to (default): " process-name
                     " (Use `M-x shime-choose-buffer-ghci-process` to change.)")))
        (buffer-cabal-process-was-set
         . (lambda (process-name)
             (concat "Buffer Cabal process set to: " process-name)))
        (buffer-cabal-process-was-set-default
         . (lambda (process-name)
             (concat "Buffer Cabal process set to (default): " process-name
                     " (Use `M-x shime-choose-buffer-cabal-process` to change.)")))
        (choose-buffer-session . "Choose session for this buffer: ")
        (enter-session-name-exists
         . "Session already exists, please enter a different session name: ")
        (session-already-started . ,(concat "Shime session(s) already started. "
                                            "Start a new session? "))
        (recieved-data-from-rogue-process
         . (lambda (process) (concat "Recieved data from rogue process " process)))
        (recieved-data-from-unattached-process
         . (lambda (process) (concat "Recieved data from unattached process " process)))
        (recieved-data-for-inactive-session
         . (lambda (process session)
             (concat "Recieved data from process " process
                     " on inactive session " session)))))

(defvar shime-languages
  "All the available languages. Re-evaluate this when
 updating an individual language at runtime.")
(setq shime-languages `(("en" . ,shime-strings-en)))

(defvar shime-cabal-commands
  '("install"
    "update"
    "list"
    "info"
    "upgrade"
    "fetch"
    "unpack"
    "check"
    "sdist"
    "upload"
    "report"
    "init"
    "configure"
    "build"
    "copy"
    "haddock"
    "clean"
    "hscolour"
    "register"
    "test"
    "help"))

(defun shime-set-cabal-commands ()
  "Parse 'cabal --help' and set `shime-cabal-commands'.
If cabal doesn't exist, `shime-cabal-commands' is left
unchanged."
  (with-temp-buffer
    (insert (shell-command-to-string "cabal --help"))
    (goto-char (point-min))

    (when (re-search-forward "cabal" nil t 2)
      (narrow-to-region (re-search-forward "Commands:\n")
                        (re-search-forward "^\n"))
      (goto-char (point-min))

      (let (cmds)
        (while (re-search-forward "^  \\([a-z]+\\)" nil t)
          (push (match-string 1) cmds))
        (when cmds (setq shime-cabal-commands (reverse cmds)))))))

;; Globals

(defvar shime-sessions '()
  "List of sessions.")

(defvar shime-processes '()
  "List of Shime processes.")

(defvar shime-buffers '()
  "List of Shime buffers.")

;; Data types

(defstruct
  (shime-config
   (:constructor
    make-shime-config
    (&key (language shime-default-language)
          name
          cabal-load-path)))
  language
  name
  cabal-load-path)

(defstruct
  (shime-session
   (:constructor
    make-shime-session
    (&key (name)
          (config)
          (processes '())
          (buffers '())
          (active-p nil))))
  name
  config
  processes
  buffers
  active-p)

(defstruct
  (shime-process
   (:constructor
    make-shime-process
    (&key program-path
          name
          session
          filter
          sentinel
          process
          buffer
          type
          pwd
          data
          block-data)))
  program-path
  name
  session
  filter
  sentinel
  process
  buffer
  type
  pwd
  data
  block-data)

(defstruct
  (shime-buffer
   (:constructor
    make-shime-buffer
    (&key name buffer session processes ghci-process)))
  name
  buffer
  session
  processes
  ghci-process)

(defun shime-make-session (name config)
  "Make a Session object."
  (let ((session (make-shime-session
                  :config config
                  :name name
                  :processes '()
                  :buffers '()
                  :active-p nil)))
    (if (assoc name shime-sessions)
        (error (concat "Unable to make Shime session named " name ", already exists."))
      (progn (add-to-list 'shime-sessions (cons name session))
             session))))

(defun shime-make-config (name cabal-load-path)
  "Make a Shime config object."
  (make-shime-config :name name
                     :cabal-load-path cabal-load-path))

(defun shime-start-process-for-shime-process (process)
  "Start a process using the details given in the shime-process
object and attach itself to it."
  (let* ((process-connection-type nil)
         (path (shime-executable-find (shime-process-program-path process)))
         (process-ref (start-process (shime-process-name process) nil path)))
    (set-process-filter process-ref (shime-process-filter process))
    (set-process-sentinel process-ref (shime-process-sentinel process))
    (setf (shime-process-process process) process-ref)))

(defun shime-make-process (session name program-path filter sentinel type pwd)
  "Make a Shime process object."
  (let ((process (make-shime-process
                  :program-path program-path
                  :name name
                  :session session
                  :filter filter
                  :sentinel sentinel
                  :process nil
                  :type type
                  :pwd pwd
                  :data ""
                  :block-data "")))
    (shime-start-process-for-shime-process process)
    (add-to-list 'shime-processes (cons name process))
    process))

(defun shime-make-buffer (session name)
  "Make a Shime buffer object associated with a session."
  (if (get-buffer name)
      ;; TODO: Look up to see if there is an existing Shime
      ;; session for that buffer, if not, offer to delete or
      ;; usurp the buffer.
      (error (concat "Unable to make Shime buffer named " name ", already exists."))
    (let ((buffer (make-shime-buffer
                   :name name
                   :buffer (get-buffer-create name)
                   :session session
                   :processes '()
                   :ghci-process nil)))
      (add-to-list 'shime-buffers (cons name buffer))
      (with-current-buffer (shime-buffer-buffer buffer)
        (kill-all-local-variables)
        (use-local-map shime-mode-map)
        (setq major-mode 'shime-mode)
        (setq mode-name "Shime")
        (run-mode-hooks 'shime-mode-hook))
      buffer)))

;; Interactive procedures

;;;###autoload
(defun shime ()
  "Start a Shime session."
  (interactive)
  (if (null shime-sessions)
      (shime-start-session
       :name shime-default-session-name
       :config (shime-make-config shime-default-session-name
                                  nil))
    (shime-maybe-start-session)))

(defun shime-start-named-session ()
  "Start a session with a given name."
  (interactive)
  (let ((name (shime-prompt-for-session-name)))
    (shime-start-session
     :name name
     :config (shime-make-config name nil))))

(defun shime-kill-session ()
  "Kill a Shime session and all associated processes and buffers."
  (interactive)
  (shime-kill-session-by-name
   (ido-completing-read (shime-string 'kill-session)
                        (mapcar 'car shime-sessions))))

(defun shime-kill-buffer ()
  "Kill a Shime buffer."
  (interactive)
  (shime-kill-buffer-by-name
   (ido-completing-read (shime-string 'kill-buffer)
                        (mapcar 'car shime-buffers))))

(defun shime-kill-process ()
  "Kill a Shime process."
  (interactive)
  (shime-kill-process-by-name
   (ido-completing-read (shime-string 'kill-process)
                        (mapcar 'car shime-processes))))

(defun shime-choose-load-root ()
  "Prompt to set the root load path (defaults to current directory)."
  (interactive)
  (shime-with-buffer-ghci-process
   process
   (shime-prompt-load-root process shime-load-root)))

(defun shime-choose-cabal-root ()
  "Prompt to set the root Cabal path (defaults to current directory)."
  (interactive)
  (shime-with-buffer-ghci-process
   process
   (shime-prompt-cabal-root process "")))

(defun shime-cabal-configure ()
  "Run the Cabal configure command."
  (interactive)
  (shime-cabal-command "configure"))

(defun shime-cabal-build ()
  "Run the Cabal build command."
  (interactive)
  (shime-cabal-command "build"))

(defun shime-cabal-clean ()
  "Run the Cabal clean command."
  (interactive)
  (shime-cabal-command "clean"))

(defun shime-cabal-install ()
  "Run the Cabal install command."
  (interactive)
  (shime-cabal-command "install"))

(defun shime-cabal-ido (&optional custom)
  "Interactively choose a cabal command to run."
  (interactive "P")
  (let ((command (ido-completing-read "Command: " shime-cabal-commands)))
    (if custom
        (shime-cabal-command (read-from-minibuffer "Command: " command))
      (shime-cabal-command command))))

(defun shime-choose-buffer-session-or-default ()
  "Choose the session for this buffer or just default if there's only one session."
  (interactive)
  (shime-with-any-session
   (if (= (length shime-sessions) 1)
       (progn (setq shime-session-of-buffer (caar shime-sessions))
              (message (funcall (shime-string 'buffer-session-was-set-default)
                                shime-session-of-buffer))
              shime-session-of-buffer)
     (shime-choose-buffer-session))))

(defun shime-choose-buffer-session ()
  "Choose the session for this buffer."
  (interactive)
  (shime-with-any-session
   (setq shime-session-of-buffer
         (ido-completing-read (shime-string 'choose-buffer-session)
                              (mapcar 'car shime-sessions))))
  (message (funcall (shime-string 'buffer-session-was-set) shime-session-of-buffer))
  shime-session-of-buffer)

(defun shime-choose-buffer-ghci-process-or-default ()
  "Choose the buffer for this buffer or just default if there's only one buffer."
  (interactive)
  (shime-with-session
   session
   (if (= (length (shime-session-ghci-processes session)) 1)
       (progn (setq shime-ghci-process-of-buffer
                    (shime-process-name
                     (car (shime-session-ghci-processes session))))
              (message (funcall (shime-string
                                 'buffer-ghci-process-was-set-default)
                                shime-ghci-process-of-buffer))
              shime-ghci-process-of-buffer)
     (shime-choose-buffer-ghci-process))))

(defun shime-choose-buffer-ghci-process ()
  "Choose the buffer for this buffer."
  (interactive)
  (shime-with-session
   session
   (setq shime-ghci-process-of-buffer
         (ido-completing-read (shime-string 'choose-buffer-ghci-process)
                              (mapcar 'shime-process-name
                                      (shime-session-ghci-processes session)))))
  (message (funcall (shime-string 'buffer-ghci-process-was-set)
                    shime-ghci-process-of-buffer))
  shime-ghci-process-of-buffer)

(defun shime-choose-buffer-cabal-process-or-default ()
  "Choose the buffer for this buffer or just default if there's only one buffer."
  (interactive)
  (shime-with-session
   session
   (if (= (length (shime-session-cabal-processes session)) 1)
       (progn (setq shime-cabal-process-of-buffer
                    (shime-process-name
                     (car (shime-session-cabal-processes session))))
              (message (funcall (shime-string
                                 'buffer-cabal-process-was-set-default)
                                shime-cabal-process-of-buffer))
              shime-cabal-process-of-buffer)
     (shime-choose-buffer-cabal-process))))

(defun shime-choose-buffer-cabal-process ()
  "Choose the buffer for this buffer."
  (interactive)
  (shime-with-session
   session
   (setq shime-cabal-process-of-buffer
         (ido-completing-read (shime-string 'choose-buffer-cabal-process)
                              (mapcar 'shime-process-name
                                      (shime-session-cabal-processes session)))))
  (message (funcall (shime-string 'buffer-cabal-process-was-set)
                    shime-cabal-process-of-buffer))
  shime-cabal-process-of-buffer)

(defun shime-cabal-command (cmd)
  "Run a cabal command."
  (interactive)
  (shime-with-buffer-cabal-process
   process
   (progn
     (when (buffer-modified-p) (save-buffer))
     (if (shime-process-pwd process)
         (shime-cabal-send-cmd process cmd)
       (progn (shime-prompt-cabal-root
               process
               (file-name-directory (buffer-file-name)))
              (shime-cabal-command cmd))))))

(defun shime-echo-command (buffer str)
  "Insert STR into BUFFER with the shime-interactive-command face."
  (with-current-buffer (shime-buffer-buffer buffer)
    (shime-delete-line)
    (shime-buffer-echo buffer
                       (propertize str 'face 'shime-interactive-command))))

(defun shime-load-file ()
  "Load the file associated with the current buffer with the
current session GHCi process."
  (interactive)
  (shime-with-buffer-ghci-process
   process
   (let* ((file (buffer-file-name))
          (file-dir (file-name-directory file))
          (proc-buffer (shime-process-buffer process)))
     (when (buffer-modified-p) (save-buffer))
     (if (shime-process-pwd process)
         (progn
           (unless (shime-relative-to (shime-process-pwd process) file-dir)
             (when (shime-ask-change-root)
               (shime-prompt-load-root process file-dir)))
           (shime-echo-command proc-buffer (format "load %s\n" file))
           (shime-ghci-send-expression process ":set -fobject-code")
           (shime-ghci-send-expression process (concat ":load " file)))
       (shime-set-load-root process file-dir)
       (shime-load-file)))))

(defun shime-reset-everything-because-it-broke ()
  "Reset everything because it broke."
  (interactive)
  (setq shime-sessions nil)
  (setq shime-buffers nil)
  (setq shime-processes nil))

;; Key binding handlers

(defun shime-key-home ()
  "Handle the home key press."
  (interactive)
  (goto-char (line-beginning-position))
  (search-forward-regexp shime-ghci-prompt-regex
                         (line-end-position)
                         t
                         1))

(defun shime-key-ret ()
  "Handle the return key press."
  (interactive)
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (p (save-excursion
              (goto-char start)
              (search-forward-regexp shime-ghci-prompt-regex
                                     end
                                     t
                                     1)))
         (buffer (assoc (buffer-name) shime-buffers)))
    (when buffer
      (let ((process (shime-get-shime-buffer-ghci-process (cdr buffer))))
        (when process
          (if p
              (let ((line (buffer-substring-no-properties p end)))
                (shime-history-ensure-created)
                (unless (string= "" line)
                  (setq shime-history-of-buffer
                        (cons line shime-history-of-buffer))
                  (setq shime-history-index-of-buffer -1))
                (shime-buffer-ghci-send-expression
                 (cdr buffer) process line))
            ;; If we're not at a prompt, send an empty line to
            ;; the REPL, this'll trigger it sending a new prompt,
            ;; which is probably what we want. At least in the
            ;; case of M-x erase buffer.
            ;; TODO: take another look at this to re-evaluate.
            (shime-buffer-ghci-send-expression
             (cdr buffer) process "")))))))

(defun shime-key-tab ()
  "Handle the tab key press."
  (interactive)
  ;; Do something.
  )

(defun shime-key-del ()
  "Handle the backspace key press."
  (interactive)
  (unless (looking-back shime-ghci-prompt-regex (line-beginning-position))
    (backward-delete-char 1)))

(defun shime-key-history-prev ()
  "Show previous history item."
  (interactive)
  (shime-history-toggle 1))

(defun shime-key-history-next ()
  "Show previous history item."
  (interactive)
  (shime-history-toggle -1))

;; Macros

(defmacro shime-with-process-buffered-lines (process input line-name &rest body)
  (let ((lines (gensym))
        (parsed-lines (gensym))
        (remaining-input (gensym)))
    `(let* ((buffer (shime-process-buffer ,process))
            (,lines (split-string (concat (shime-process-data ,process)
                                          ,input)
                                  "[\r\n]"))
            (,parsed-lines (butlast ,lines))
            (,remaining-input (car (or (last ,lines) '("")))))
       (with-current-buffer (shime-buffer-buffer buffer)
         (unless (or (null ,parsed-lines)
                     (string= (shime-process-data ,process) ""))
           (shime-delete-line))
         (mapc (lambda (,line-name) ,@body)
               ,parsed-lines)

         (if (string-match shime-ghci-prompt-regex ,remaining-input)
	      (progn (setf (shime-process-data ,process) "")
		     (mapc (lambda (,line-name) ,@body) '("")))
           (setf (shime-process-data ,process) ,remaining-input))

         (when (not (string= ,remaining-input ""))
	   (shime-delete-line)
	   (shime-buffer-echo
	    buffer
	    ;; Colorize the prompt with `shime-ghci-prompt' and set
	    ;; it to read-only.  Set `rear-nonsticky' so properties
	    ;; don't bleed onto user input.
	    (if (string-match shime-ghci-prompt-regex ,remaining-input)
		(let ((prompt (substring ,remaining-input
					 (match-beginning 0)
					 (match-end 0)))
		      (rest (substring ,remaining-input (match-end 0))))
		  (concat (propertize prompt
				      'face 'shime-ghci-prompt
				      'read-only t
				      'rear-nonsticky t)
			  rest))
	      ,remaining-input)))))))

(defmacro shime-with-process-session (process process-name session-name &rest body)
  "Get the process object and session for a processes."
  `(let ((process (assoc (process-name ,process) shime-processes)))
     (if (not process)
         (message (funcall (shime-string 'recieved-data-from-rogue-process)
                           (process-name ,process)))
       (let ((session (shime-process-session (cdr process))))
         (if (not session)
             (message (funcall (shime-string 'recieved-data-from-unattached-process)
                               (process-name ,process)))
           (if (not (shime-session-active-p session))
               (message (funcall (shime-string 'recieved-data-for-inactive-session)
                                 (process-name ,process) ""))
             (let ((,session-name session)
                   (,process-name (cdr process)))
               ,@body)))))))

(defmacro shime-with-any-session (&rest body)
  "The code this call needs a session. Ask to create one if needs be."
  `(if (null shime-sessions)
       (if (y-or-n-p (shime-string 'start-shime))
           (progn (shime)
                  ,@body)
         (message (shime-string 'needed-a-session)))
     ,@body))

(defmacro shime-with-session (name &rest body)
  "The code this call needs a session. Ask to create one if needs be."
  `(shime-with-any-session
    (if (= 1 (length shime-sessions))
        (let ((,name (cdar shime-sessions)))
          ,@body)
      (let ((,name (assoc (shime-choose-session) shime-sessions)))
        (if ,name
            (let ((,name (cdr ,name)))
              ,@body)
          (message (shime-string 'needed-a-session)))))))

;; TODO: Maybe a bit more interactivity.
(defmacro shime-with-buffer-ghci-process (name &rest body)
  (let ((sym (gensym)) (cons (gensym)))
    `(let ((,sym (shime-get-buffer-ghci-process)))
       (if ,sym
           (let ((,cons (assoc ,sym shime-processes)))
             (if ,cons
                 (let ((,name (cdr ,cons)))
                   ,@body)))))))

;; TODO: Maybe a bit more interactivity.
(defmacro shime-with-buffer-cabal-process (name &rest body)
  (let ((sym (gensym)) (cons (gensym)))
    `(let ((,sym (shime-get-buffer-cabal-process)))
       (if ,sym
           (let ((,cons (assoc ,sym shime-processes)))
             (if ,cons
                 (let ((,name (cdr ,cons)))
                   ,@body)))))))

;; Procedures

(defun shime-history-ensure-created ()
  "Ensure the local variable for history is created."
  (unless (default-boundp 'shime-history-of-buffer)
    (setq shime-history-of-buffer '(""))
    (setq shime-history-index-of-buffer 0)
    (make-local-variable 'shime-history-of-buffer)
    (make-local-variable 'shime-history-index-of-buffer)))

(defun shime-history-toggle (direction)
  "Toggle the prompt contents by cycling the history in DIRECTION."
  (shime-history-ensure-created)
  (setq shime-history-index-of-buffer
        (+ shime-history-index-of-buffer
           direction))
  (shime-buffer-clear-prompt)
  (insert (nth (mod shime-history-index-of-buffer
                    (length shime-history-of-buffer))
               shime-history-of-buffer)))

(defun shime-buffer-clear-prompt ()
  "Clear the current prompt."
  ;; TODO: Maybe check that we're in an actual Shime buffer, in
  ;; case a maurading fool enters and splashes water over
  ;; everyone's dogs.
  (goto-char (point-max))
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (goto-char start)
    (let* ((p (search-forward-regexp shime-ghci-prompt-regex
                                     end
                                     t
                                     1)))
      (when p
        ;; TODO: I don't like this so much, not like a clear
        ;; spring in a Yorkshire morning, but it actually seems
        ;; sufficient.
        (delete-region p end)))))

(defun shime-get-shime-buffer-ghci-process (buffer)
  (let ((process (shime-buffer-ghci-process buffer)))
    (if process
        process
      (if (null (shime-buffer-processes buffer))
          (progn (message (shime-string 'buffer-no-processes))
                 nil)
        (let ((ghci-processes
               (remove-if (lambda (process)
                            (not (eq (shime-process-type process)
                                     'ghci)))
                          (shime-buffer-processes buffer))))
          (if (= 1 (length ghci-processes))
              (progn (setf (shime-buffer-ghci-process buffer) (car ghci-processes))
                     (car ghci-processes))
            (let ((process-name (ido-completing-read
                                 (shime-string 'choose-buffer-process)
                                 (mapcar 'shime-process-name ghci-processes))))
              (let ((process (assoc process-name shime-processes)))
                (if process
                    (progn (setf (shime-buffer-ghci-process buffer) (cdr process))
                           (cdr process))
                  (shime-get-shime-buffer-ghci-process buffer))))))))))

(defun shime-choose-session ()
  "Ask the user to choose from the list of sessions."
  (ido-completing-read (shime-string 'choose-session)
                       (mapcar 'car shime-sessions)))

(defun shime-start-new-ghci-process (session name &optional buffer)
  "Start a new GHCi process and attach to the given session."
  (let ((ghci-process (shime-make-ghci-process session
                                               name
                                               shime-default-ghci-path
                                               config)))
    (shime-attach-process-to-session session ghci-process)
    (when buffer (shime-attach-process-to-buffer ghci-process buffer))
    ghci-process))

(defun shime-start-new-cabal-process (session name &optional buffer)
  "Start a new Cabal process and attach to the given session."
  (let ((cabal-process (shime-make-cabal-process session
                                                 name
                                                 shime-default-shell-path
                                                 config)))
    (shime-attach-process-to-session session cabal-process)
    (when buffer (shime-attach-process-to-buffer cabal-process buffer))
    cabal-process))

(defun shime-start-new-buffer (session name)
  "Start a new buffer and attach it to the given session."
  (let ((buffer (shime-make-buffer session name)))
    (shime-attach-buffer-to-session session buffer)
    buffer))

(defun shime-get-buffer-ghci-process (&optional buffer)
  "Get the GHCi process of BUFFER.
If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if (and (default-boundp 'shime-ghci-process-of-buffer)
	     (assoc shime-ghci-process-of-buffer shime-processes))
	shime-ghci-process-of-buffer
      (setq shime-ghci-process-of-buffer nil)
      (make-local-variable 'shime-ghci-process-of-buffer)
      (shime-choose-buffer-ghci-process-or-default))))

(defun shime-get-buffer-cabal-process ()
  "Get the Cabal process of the current buffer."
  (if (and (default-boundp 'shime-cabal-process-of-buffer)
           (assoc shime-cabal-process-of-buffer shime-processes))
      shime-cabal-process-of-buffer
    (progn (set (make-local-variable 'shime-cabal-process-of-buffer) nil)
           (shime-choose-buffer-cabal-process-or-default))))

(defun shime-get-buffer-session ()
  "Get the session of the current buffer."
  (if (and (default-boundp 'shime-session-of-buffer)
           (assoc shime-session-of-buffer shime-sessions))
      shime-session-of-buffer
    (progn (setq shime-session-of-buffer nil)
           (make-local-variable 'shime-session-of-buffer)
           (shime-choose-buffer-session-or-default))))

(defun shime-maybe-start-session ()
  "Maybe start a new session."
  (when (y-or-n-p (shime-string 'session-already-started))
    (shime-start-named-session)))

(defun shime-prompt-for-session-name (&optional exists)
  "Prompt for the name of a session."
  (let ((name (read-from-minibuffer
               (shime-string
                (if exists 'enter-session-name-exists 'enter-session-name)))))
    (if (assoc name shime-sessions)
        (shime-prompt-for-session-name t)
      name)))

(defun* shime-start-session (&key name (config (make-shime-config)))
  "Start a normal session."
  (let* ((session (shime-make-session name config))
         (buffer (shime-start-new-buffer session (shime-new-buffer-name session))))
    (shime-start-new-ghci-process session
                                  (shime-new-process-name session "ghci")
                                  buffer)
    (shime-start-new-cabal-process session
                                   (shime-new-process-name session "cabal")
                                   buffer)
    (setf (shime-session-active-p session) t)
    session))

(defun shime-new-buffer-name (session)
  "Generate a unique buffer name to be used in a session."
  ;; FIXME: Make sure it's actually unique, better algo. Most
  ;; cases it will be okay for now.
  (if (null (shime-session-buffers session))
      (concat "*" (shime-session-name session) "*")
    (concat "*" (shime-session-name session)
            "-" (number-to-string (length (shime-session-buffers session)))
            "*")))

(defun shime-new-process-name (session prefix)
  "Generate a unique process name to be used in a session."
  ;; TODO: Elsewhere ensure that a process cannot be attached to
  ;; a session if that name already exists. Doesn't make sense.
  ;; FIXME: Make sure it's actually unique when generating
  ;; it. Most cases it will be okay for now.
  (if (null (shime-session-processes session))
      (concat (shime-session-name session) "-" prefix)
    (concat (shime-session-name session) "-" prefix "-"
            (number-to-string (1- (length (shime-session-processes session)))))))

(defun shime-attach-process-to-buffer (process buffer)
  "Bidirectionally attach a process to a buffer."
  (when process
    (setf (shime-process-buffer process) buffer)
    (setf (shime-buffer-processes buffer)
          (cons process (shime-buffer-processes buffer)))))

(defun shime-detach-process-from-buffer (process buffer)
  "Bidiretionally detach a process from a buffer."
  (when process
    (setf (shime-process-buffer process) nil)
    (setf (shime-buffer-processes buffer)
          (delete-if (lambda (proc)
                       (string= (shime-process-name proc)
                                (shime-process-name process)))
                     (shime-buffer-processes buffer)))))

(defun shime-attach-process-to-session (session process)
  "Bidirectionally attach a process to a session."
  (setf (shime-session-processes session)
        (cons process (shime-session-processes session)))
  (setf (shime-process-session process) session))

(defun shime-detach-process-from-session (process session)
  "Bidirectionally detach a process from a session."
  (setf (shime-session-processes session)
        (delete-if (lambda (proc)
                     (string= (shime-process-name proc)
                              (shime-process-name process)))
                   (shime-session-processes session)))
  (setf (shime-process-session process) nil))

(defun shime-attach-buffer-to-session (session buffer)
  "Bidirectionally attach a buffer to a session."
  (setf (shime-session-buffers session)
        (cons buffer (shime-session-buffers session)))
  (setf (shime-buffer-session buffer) session))

(defun shime-detach-buffer-from-session (buffer session)
  "Bidirectionally detach a buffer from a session."
  (mapc (lambda (process)
            (shime-detach-process-from-buffer process buffer))
          (shime-buffer-processes buffer))
  (setf (shime-session-buffers session)
        (delete-if (lambda (buf)
                     (string= (shime-buffer-name buf)
                              (shime-buffer-name buffer)))
                   (shime-session-buffers session)))
  (setf (shime-buffer-session buffer) nil))

;; Cabal procedures

(defun shime-session-cabal-processes (session)
  (remove-if (lambda (process)
               (not (eq (shime-process-type process) 'cabal)))
             (shime-session-processes session)))

(defun shime-make-cabal-process (session name program-path config)
  "Make a Cabal process."
  (shime-make-process
   session
   name
   program-path
   #'shime-cabal-filter
   #'shime-cabal-sentinel
   'cabal
   nil))

(defun shime-cabal-filter-handle-input (session process input)
  "Handle input from the process on a given session and process."
  (shime-with-process-buffered-lines
   process input line
   (shime-buffer-echo
    buffer
    (cond
     ;; Redisplay the prompt after cabal finishes.
     ;;
     ;; TODO: Put this in the sentinel.  The shime-cabal-sentinel
     ;; doesn't seem to work at the moment.
     ((string-match (shime-string 'cabal-command-finished) line)
      (with-current-buffer (shime-buffer-buffer buffer)
        (let ((ghci-proc (shime-get-shime-buffer-ghci-process buffer)))
          (shime-ghci-send-expression ghci-proc ""))
        ""))
     (t (concat line "\n"))))))

(defun shime-cabal-filter (process. input)
  "The process filter for Cabal processes."
  (shime-with-process-session
   process. process session
   (shime-cabal-filter-handle-input session process input)))

(defun shime-cabal-sentinel (process event)
  "Sentinel for Cabal processes."
  )

(defface shime-interactive-command
  '((t :inherit 'font-lock-keyword-face))
  "Face for cabal commands."
  :group 'shime)

(defun shime-cabal-send-cmd (process cmd)
  "Send an expression."

  (let ((buffer (shime-process-buffer process))
        (proc (shime-process-process process))
        (cabal-cmd (format "%s %s\n" shime-cabal-program-path cmd)))

    ;; Erase the prompt and color the command to show that the cabal
    ;; command is separate from GHCi.
    (shime-echo-command buffer (format "cabal %s\n" cmd))

    (process-send-string proc
                         (concat
                          cabal-cmd
                          ;; TODO: Something better than this.
                          "echo \"" (shime-string 'cabal-command-finished) "\"\n"
                          ))))

(defun shime-cabal-send-line (process line)
  "Send an expression."
  (process-send-string (shime-process-process process)
                       (concat line "\n")))

;; GHCi procedures

(defun shime-session-ghci-processes (session)
  (remove-if (lambda (process)
               (not (eq (shime-process-type process) 'ghci)))
             (shime-session-processes session)))

(defun shime-ghci-send-expression (process expr)
  "Send an expression."
  (process-send-string (shime-process-process process) (concat expr "\n")))

(defun shime-make-ghci-process (session name program-path config)
  "Make a GHCi process."
  (shime-make-process
   session
   name
   program-path
   #'shime-ghci-filter
   #'shime-ghci-sentinel
   'ghci
   nil))

(defun shime-buffer-ghci-send-expression (buffer process expr)
  "Send an expression to the (first) GHCi process associated with this buffer."
  (shime-buffer-echo buffer "\n")
  (shime-ghci-send-expression process expr))

(defun shime-ghci-filter (process. input)
  "The process filter for GHCi processes."
  (shime-with-process-session
   process. process session
   (shime-ghci-filter-handle-input session process input)))

(defface shime-ghci-error
  '((t :inherit 'compilation-error))
  "Face for error messages."
  :group 'shime)

(defface shime-ghci-warning
  '((t :inherit 'compilation-warning))
  "Face for warning messages."
  :group 'shime)

(defface shime-ghci-prompt
  '((t :inherit 'font-lock-function-name-face))
  "Face for the Shime GHCi prompt."
  :group 'shime)

(defun shime-ghci-filter-handle-input (session process input)
  "Handle input from the process on a given session and process."
  (shime-with-process-buffered-lines
   process input line
   (let* ((err "^\\(.+?:[0-9]+:[0-9]+: ?\\)")
          (block-data (shime-process-block-data process))
          (block-data-p (not (string= block-data "")))
          (was-error nil)
          (block-data-flat
           (if shime-collapse-errors
               (replace-regexp-in-string
                "[\r\n ]+" " "
                (replace-regexp-in-string "\nIn the.+$" "" block-data))
             block-data))
          (warning-match (string-match "^.+?:[0-9]+:[0-9]+: Warning" block-data-flat)))
     (if (or (string-match err line)
             (and block-data-p (string-match "^    " line)))
         (setf (shime-process-block-data process)
               (if (not (string= "" block-data))
                   (concat block-data "\n" line)
                 line))
       (when block-data-p
         (shime-buffer-echo buffer
                            (propertize (concat block-data-flat)
                                        'face (if warning-match
                                                  'shime-ghci-warning
                                                'shime-ghci-error)))
         (shime-buffer-echo buffer "\n")
         (setf (shime-process-block-data process) "")
         (setq was-error t))
       (shime-buffer-echo buffer (concat line (unless (looking-back "\n") "\n")))))))

(defun shime-trim-flat (str)
  (replace-regexp-in-string
   "[\r\n ]+"
   " "
   block-data))

(defun shime-delete-line ()
  "Delete the current line."
  (delete-region (line-beginning-position)
                 (line-end-position)))

(defun shime-ghci-sentinel (process event)
  "Sentinel for GHCi processes."
  (cond ((or (string-match "finished" event)
             (string-match "segmentation fault" event))
         (shime-ghci-handle-finished process))
        (t (print process)
           (print event))))

(defun shime-ghci-handle-finished (process.)
  "Handle the event of GHCi dying or just closing."
  ;; TODO: It's probably worth, later on, adding some hooks to
  ;; this, one might want to know when GHCi bails and act.
  (when (y-or-n-p (funcall (shime-string 'ghci-died-restart?)
                           (process-name process.)))
    (shime-with-process-session
     process. process session
     (progn (message (shime-string 'restarting-ghci-process))
            (shime-start-process-for-shime-process process)))))

(defun shime-kill-session-by-name (name)
  "Kill a Shime session and all associated buffers and processes."
  (let ((session (assoc name shime-sessions)))
    (when session
      (mapc (lambda (buffer) (shime-kill-buffer-by-name (shime-buffer-name buffer)))
	    (shime-session-buffers (cdr session)))
      (mapc (lambda (proc) (shime-kill-process-by-name (shime-process-name proc)))
	    (shime-session-processes (cdr session)))
      (setf (shime-session-active-p (cdr session)) nil)
      (setq shime-sessions
            (delete-if (lambda (keyvalue)
                         (string= (car keyvalue) name))
                       shime-sessions)))))

(defun shime-kill-process-by-name (name)
  "Kill a Shime process and detach it from its buffer, and detach from session."
  (let ((process (assoc name shime-processes)))
    (when process
      (let ((session (shime-process-session (cdr process))))
        (when session
          (shime-detach-process-from-session (cdr process) session)
          (let ((buffer (shime-process-buffer (cdr process))))
            (when buffer
              (shime-detach-process-from-buffer session buffer)))))
      (setq shime-processes
            (delete-if (lambda (keyvalue)
                         (string= (car keyvalue) name))
                       shime-processes))))
  (when (get-process name)
    (delete-process name)))

(defun shime-kill-buffer-by-name (name)
  "Kill a Shime buffer and detach it from the session, and detach any processes."
  (let ((buffer (assoc name shime-buffers)))
    (when buffer
      (let ((session (shime-buffer-session (cdr buffer))))
        (when session
          (shime-detach-buffer-from-session (cdr buffer) session)))
      (setq shime-buffers
            (delete-if (lambda (keyvalue)
                         (string= (car keyvalue) name))
                       shime-buffers))))
  (when (get-buffer name)
    (kill-buffer name)))

(defun shime-buffer-echo (buffer str)
  "Echo something into the buffer of a buffer object."
  (with-current-buffer (shime-buffer-buffer buffer)
    (goto-char (point-max))
    (with-selected-window (display-buffer (shime-buffer-buffer buffer) nil 'visible)
      (insert str)
      (goto-char (point-max)))))

;; Functions

(defun* shime-string (n &key (lang shime-default-language))
  "Look-up a string with the current language."
  (let ((entry (assoc n (assoc lang shime-languages))))
    (if entry
        (cdr entry)
      (error (concat
              "Unable to retrieve language entry for "
              (symbol-name n)
              " from language set "
              lang
              ".")))))

;; IO/paths/filesytem

(defun shime-executable-find (name)
  "Find the program path, and prompt for a new one until it can find one."
  (if (executable-find name)
      name
    (shime-executable-find
     (read-from-minibuffer (funcall (shime-string 'program-not-found)
                                    name)
                           name))))

(defun shime-set-load-root (process root)
  (setf (shime-process-pwd process) root)
  (shime-buffer-ghci-send-expression
   (shime-process-buffer process)
   process
   (concat ":cd " root)))

(defun shime-prompt-load-root (process def)
  "Prompt to set the root path with a default value."
  (interactive)
  (shime-set-load-root
   process
   (read-from-minibuffer (shime-string 'new-load-root)
                         def)))

(defun shime-set-cabal-root (process root)
  (setf (shime-process-pwd process) root)
  (shime-cabal-send-line
   process
   (concat "cd " root)))

(defun shime-prompt-cabal-root (process def)
  "Prompt to set the root Cabal path with a default value."
  (interactive)
  (shime-set-cabal-root
   process
   (read-from-minibuffer (shime-string 'new-cabal-root)
                         def)))

(defun shime-ask-change-root ()
  (y-or-n-p (shime-string 'ask-change-root)))

(defun shime-relative-to (a b)
  "Is a path b relative to path a?"
  (shime-is-prefix-of (directory-file-name a)
                      (directory-file-name b)))

(defun shime-is-prefix-of (a b)
  "Is one string a prefix of another?"
  (and (<= (length a) (length b))
       (string= (substring b 0 (length a)) a)))

(defun shime-strip-/ (a)
  "Strip trailing slashes."
  (replace-regexp-in-string "[/\\\\]+$" "" a))

(defun shime-/ (a b)
  "Append two paths."
  (concat (shime-strip-/ a)
          "/"
          (replace-regexp-in-string "^[/\\\\]+" "" b)))

(defun shime-path-filename (path)
  "Get the filename part of a path."
  (car (last (shime-split-path path))))

;; Haven't seen an Elisp function that does this.
(defun shime-path-directory (path)
  "Get the directory part of a path."
  (if (file-directory-p path)
      path
    ;; I think `/' works fine on Windows.
    (reduce #'shime-/ (butlast (shime-split-path path)))))

(defun shime-split-path (path)
  "Split a filename into path segments."
  (split-string path "[/\\\\]"))

(provide 'shime)

;;; shime.el ends here
