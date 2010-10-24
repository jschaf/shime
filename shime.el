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

;; Mode definition

(defvar shime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'shime-key-ret)
    (define-key map (kbd "C-j") 'shime-key-ret)
    map)
  "Shime mode map.")

(define-derived-mode shime-mode nil "Shime"
  (make-local-variable 'shime-mode)
  (setq shime-mode t))

;; Customization

(defcustom shime-default-ghci-path "ghci"
  "Default GHCi path."
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

(defcustom shime-ghci-prompt-regex "^λ> \\(.*\\)"
  "Regex to match the prompt string."
  :group 'shime
  :type 'string)

;; Constants

(defvar shime-strings-en
  `((process-died . "The Shime process died. Restart it? ")
    (program-not-found
     . (lambda (name)
         (concat "Unable to find Shime program \"" name
                 "\", what's the right path? ")))
    (could-not-start . "Shime could not start.")
    (ask-change-root . "Do you want to change the root directory? ")
    (new-root . "New project root: ")
    (enter-session-name . "Session name: ")
    (kill-session . "Kill Shime session: ")
    (kill-process . "Kill Shime process: ")
    (kill-buffer . "Kill Shime buffer: ")
    (shime-ask-change-root . "Do you want to change the root directory? ")
    (shime-new-root . "New project root: ")
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
                 " on inactive session " session))))
  "English language strings.")

(defvar shime-languages `(("en" . ,shime-strings-en))
  "All the available languages. Re-evaluate this when
 updating an individual language at runtime.")

;; Globals

(defvar shime-sessions '()
  "List of sessions.")

(defvar shime-processes '()
  "List of Shime processes.")

(defvar shime-buffers '()
  "List of Shime buffers.")

;; TODO: This should be part of sessions.
(defvar shime-root nil)

;; Data types

(defstruct
  (shime-config
   (:constructor
    make-shime-config
    (&key (language shime-default-language)
          name)))
  language
  name)

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
          type)))
  program-path
  name
  session
  filter
  sentinel
  process
  buffer
  type)

(defstruct
  (shime-buffer
   (:constructor
    make-shime-buffer
    (&key name buffer session processes)))
  name
  buffer
  session
  processes)

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

(defun shime-make-config (name)
  "Make a Shime config object."
  (make-shime-config :name name))

(defun shime-make-process (session name program-path filter sentinel type)
  "Make a Shime process object."
  (let ((process-connection-type nil))
    (let ((process-ref (start-process name nil (shime-executable-find program-path))))
      (set-process-filter process-ref filter)
      (set-process-sentinel process-ref sentinel)
      (let ((process (make-shime-process 
                      :program-path program-path
                      :name name
                      :session session
                      :filter filter
                      :sentinel sentinel
                      :process process-ref
                      :type type)))
        (add-to-list 'shime-processes (cons name process))
        process))))

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
                   :processes '())))
      (add-to-list 'shime-buffers (cons name buffer))
      (with-current-buffer (shime-buffer-buffer buffer)
        (kill-all-local-variables)
        (use-local-map shime-mode-map)
        (setq major-mode-shime-mode)
        (setq mode-name "Shime")
        (run-mode-hooks 'shime-mode-hook))
      buffer)))

;; Interactive procedures

(defun shime ()
  "Start a Shime session."
  (interactive)
  (if (null shime-sessions)
      (shime-start-session
       :name shime-default-session-name
       :config (shime-make-config shime-default-session-name))
    (shime-maybe-start-session)))

(defun shime-start-named-session ()
  "Start a session with a given name."
  (interactive)
  (let ((name (shime-prompt-for-session-name)))
    (shime-start-session
     :name name
     :config (shime-make-config name))))

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

;; Key binding handlers

(defun shime-key-ret ()
  "Handle the return key press."
  (interactive)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) 
                (line-end-position)))
         (input (string-match shime-ghci-prompt-regex line)))
    (when input
      (let ((buffer (assoc (buffer-name) shime-buffers)))
        (when buffer
          ;; TODO: Come up with a nice way for processes to hook
          ;; into inputs, making this generic.
          (shime-buffer-ghci-send-expression (cdr buffer) (match-string 1 line)))))))

(defun shime-buffer-ghci-send-expression (buffer expr)
  "Send an expression to the (first) GHCi process associated with this buffer."
  (let ((process (find-if (lambda (process)
                            (equal 'ghci (shime-process-type process)))
                          (shime-buffer-processes buffer))))
    (shime-buffer-echo buffer "\n")
    (shime-ghci-send-expression process expr)))

;; TODO: Associate file buffers with specific sessions
(defun shime-load-file ()
  "Load the file associated with the current buffer."
  (interactive)
  (save-buffer)
  (if shime-root
      (let ((path (cond ((shime-relative-to shime-root (shime-buffer-directory))
                         (shime-load-file-relative))
                        ((shime-ask-change-root)
                         (shime-prompt-root (shime-buffer-directory))
                         (shime-/ shime-root (shime-buffer-filename)))
                        (t (buffer-file-name)))))
        (shime-ghci-send-expression-dwim (concat ":load " path)))
    (progn (shime-set-root (shime-buffer-directory))
           (shime-load-file))))

;; Procedures

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
         (ghci-process (shime-make-ghci-process session
                                                (concat name "-ghci")
                                                shime-default-ghci-path
                                                config))
         (buffer (shime-make-buffer session (concat "*" name "*"))))
    (setf (shime-session-active-p session) t)
    (shime-attach-process-to-session session ghci-process)
    (shime-attach-buffer-to-session session buffer)
    (shime-attach-process-to-buffer ghci-process buffer)
    session))

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
  (mapcar (lambda (process)
            (shime-detach-process-from-buffer process buffer))
          (shime-buffer-processes buffer))
  (setf (shime-session-buffers session)
        (delete-if (lambda (buf)
                     (string= (shime-buffer-name buf)
                              (shime-buffer-name buffer)))
                   (shime-session-buffers session)))
  (setf (shime-buffer-session buffer) nil))

(defun shime-ghci-send-expression-dwim (expr)
  "Send an expression to a GHCi process, just do what I mean."
  (let ((buffer (assoc (concat "*" shime-default-session-name "*") shime-buffers)))
    (if buffer
        (shime-buffer-ghci-send-expression (cdr buffer) expr)
      (progn (shime)
             (shime-ghci-send-expression-dwim expr)))))

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
   'ghci))

(defun shime-ghci-filter (process. input)
  "The process filter for GHCi processes."
  (let ((process (assoc (process-name process.) shime-processes)))
    (if (not process)
        (message (funcall (shime-string 'recieved-data-from-rogue-process)
                          (process-name process.)))
      (let ((session (shime-process-session (cdr process))))
        (if (not session)
            (message (funcall (shime-string 'recieved-data-from-unattached-process)
                              (process-name process.)))
          (if (not (shime-session-active-p session))
              (message (funcall (shime-string 'recieved-data-for-inactive-session)
                                (process-name process.) ""))
            (shime-ghci-filter-handle-input session (cdr process) input)))))))

(defun shime-ghci-filter-handle-input (session process input)
  "Handle input from the process on a given session and process."
  (let ((buffer (shime-process-buffer process)))
    (shime-buffer-echo buffer input)))

(defun shime-ghci-sentinel (process event)
  "Sentinel for GHCi processes.")

(defun shime-kill-session-by-name (name)
  "Kill a Shime session and all associated buffers and processes."
  (let ((session (assoc name shime-sessions)))
    (when session
      (mapcar (lambda (buffer) (shime-kill-buffer-by-name (shime-buffer-name buffer)))
              (shime-session-buffers (cdr session)))
      (mapcar (lambda (proc) (shime-kill-process-by-name (shime-process-name proc)))
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
      (end-of-buffer))))

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

(defun shime-set-root (root)
  (setq shime-root root)
  (shime-ghci-send-expression-dwim (concat ":cd " root)))

(defun shime-choose-root ()
  (interactive)
  "Prompt to set the root path (defaults to current root)."
  (shime-prompt-root shime-root))

(defun shime-prompt-root (def)
  (interactive)
  "Prompt to set the root path with a default vaule."
  (shime-set-root (read-from-minibuffer (shime-string 'shime-new-root)
                                        def)))

(defun shime-ask-change-root ()
  (y-or-n-p (shime-string 'shime-ask-change-root)))

(defun shime-load-file-relative ()
  "Load a file relative to the current root."
  (cond ((string= (shime-strip-/ shime-root) (shime-strip-/ (shime-buffer-directory)))
         (shime-buffer-filename))
        ((shime-relative-to shime-root (shime-buffer-directory))
         (shime-/
          (shime-strings-suffix (shime-buffer-directory)
                                (concat (shime-strip-/ shime-root)
                                        "/"))
          (shime-buffer-filename)))))

(defun shime-relative-to (a b)
  "Is a path b relative to path a?"
  (shime-is-prefix-of (shime-strip-/ a) (shime-strip-/ b)))

(defun shime-strip-/ (a)
  "Strip trailing slashes."
  (replace-regexp-in-string "[/\\\\]+$" "" a))

(defun shime-/ (a b)
  "Append two paths."
  (concat (shime-strip-/ a)
          "/"
          (replace-regexp-in-string "^[/\\\\]+" "" b)))

(defun shime-strings-suffix (a b)
  "Return the suffix of of the longer of two strings."
  (substring (if (> (length a) (length b)) a b)
             (min (length a) (length b))
             (max (length a) (length b))))

(defun shime-is-prefix-of (a b)
  "Is one string a prefix of another?"
  (and (<= (length a) (length b))
       (string= (substring b 0 (length a)) a)))

(defun shime-buffer-filename ()
  "Get the filename of the buffer."
  (shime-path-filename (buffer-file-name)))

(defun shime-buffer-directory ()
  "Get the directory of the buffer."
  (shime-path-directory (buffer-file-name)))

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
