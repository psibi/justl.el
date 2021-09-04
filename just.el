(require 'transient)
(require 'cl)

(defgroup just nil
  "Justfile customization group"
  :group 'languages
  :prefix "just-"
  :link '(url-link :tag "Site" "https://github.com/psibi/just.el")
  :link '(url-link :tag "Repository" "https://github.com/psibi/just.el"))

(defcustom just-executable "just"
  "Location of just executable."
  :type 'file
  :group 'just
  :safe 'stringp)

(cl-defstruct jrecipe name args)
(cl-defstruct jarg arg default)

(defun just--process-error-buffer (process-name)
  "Return the error buffer name for the PROCESS-NAME."
  (format "*%s:err*" process-name))

(defvar just--last-command nil)

(defconst just--process-buffer "*just-process*"
  "Just process buffer name.")

(defun just--append-to-process-buffer (str)
  "Append string STR to the process buffer."
  (with-current-buffer (get-buffer-create just--process-buffer)
    (read-only-mode -1)
    (goto-char (point-max))
    (insert (format "%s\n" str))))

(defun just--log-command (process-name cmd)
  "Log the just command to the process buffer.

PROCESS-NAME is the name of the process.
CMD is the just command as a list."
  (let ((str-cmd (if (equal 'string (type-of cmd)) cmd (mapconcat #'identity cmd " "))))
    (setq just--last-command str-cmd)
    (just--append-to-process-buffer
     (format "[%s]\ncommand: %s" process-name str-cmd))))

(defun just--sentinel (process _)
  "Sentinel function for PROCESS."
  (let ((process-name (process-name process))
        (exit-status (process-exit-status process)))
    (just--append-to-process-buffer (format "[%s]\nexit-code: %s" process-name exit-status))
    (unless (eq 0 exit-status)
       (let ((err (with-current-buffer (just--process-error-buffer process-name)
                 (buffer-string))))
      (just--append-to-process-buffer (format "error: %s" err))
      (error (format "just process %s error: %s" process-name err))))))

(defun just--exec (process-name args &optional readonly)
  "Utility function to run commands in the proper context and namespace.

PROCESS-NAME is an identifier for the process.  Default to \"just\".
ARGS is a ist of arguments.
READONLY If true buffer will be in readonly mode(view-mode)."
  (when (equal process-name "")
    (setq process-name "just"))
  (let ((buffer-name (format "*%s*" process-name))
        (error-buffer (just--process-error-buffer process-name))
        (cmd (append (list "just") args)))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (when (get-buffer error-buffer)
      (kill-buffer error-buffer))
    (just--log-command process-name cmd)
    (make-process :name process-name
                  :buffer buffer-name
                  :sentinel #'just--sentinel
                  :file-handler t
                  :stderr error-buffer
                  :command cmd)
    (pop-to-buffer buffer-name)
    (if readonly
        (with-current-buffer buffer-name
          (view-mode)))))

(defun just--exec-to-string (cmd)
  "Replace \"shell-command-to-string\" to log to process buffer.

CMD is the command string to run."
  (just--log-command "just-command" cmd)
  (shell-command-to-string cmd))

(defun just--get-recipies ()
  "Get all the recipies"
  (let ((recipies (split-string (just--exec-to-string
                                 (format "just --summary")) " ")))
    recipies))

(defun just-exec-recipie ()
  "Set the namespace."
  (interactive)
  (let* ((recipies (completing-read "Recipies: " (just--get-recipies)
                                     nil nil nil nil "default")))
    (just--exec "just" (list recipies))
    ))

;; mode map
(defvar just-enhanced-mode-map
  (let ((map (make-sparse-keymap)))
    ;; global
    (define-key map (kbd "l") 'just-list-recipies)
    map)
  "Keymap for `just-mode'.")

;; (setq test "hell")

;; (bound-and-true-p nil)

;; (defun just-runner (path args)
;;   (let ((justfile-arg (if (bound-and-true-p path)
;;                           (list "--justfile" path)
;;                         nil))))
;;   (call-process just-executable))

;; (defun just-list-entries (path)
;;   (just-executable)
;;   )
