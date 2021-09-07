(require 'transient)
(require 'cl-lib)
(require 'xterm-color)
(require 's)
(require 'f)

(defgroup justl nil
  "Justfile customization group"
  :group 'languages
  :prefix "justl-"
  :link '(url-link :tag "Site" "https://github.com/psibi/just.el")
  :link '(url-link :tag "Repository" "https://github.com/psibi/just.el"))

(defcustom justl-executable "just"
  "Location of just executable."
  :type 'file
  :group 'justl
  :safe 'stringp)

(cl-defstruct jrecipe name args)
(cl-defstruct jarg arg default)

(defun just--jrecipe-has-args (jrecipe)
  "Checks if jreceipe has any arguments"
  (not (null (jrecipe-args jrecipe))))

(defun just--util-maybe (maybe default)
  "Maybe combinator"
  (if (null maybe)
  default
  maybe))

(defun just--arg-to-str (jarg)
  "Convert jarg to string to make it ready as argument"
  (format "%s=%s" (jarg-arg jarg) (just--util-maybe (jarg-default jarg) "")))

(defun just--jrecipe-get-args (jrecipe)
  "Convert jrecipe args to list of strings for process arguments"
  (let* ((recipe-args (jrecipe-args jrecipe))
         (args (just--util-maybe recipe-args (list))))
    (map 'list 'just--arg-to-str args)
  ))

(defun just--process-error-buffer (process-name)
  "Return the error buffer name for the PROCESS-NAME."
  (format "*%s:err*" process-name))

(defun justl--pop-to-buffer (name)
  "Utility function to pop to buffer or create it.

NAME is the buffer name."
  (unless (get-buffer name)
    (get-buffer-create name))
  (pop-to-buffer-same-window name))

(defvar just--last-command nil)

(defconst just--process-buffer "*just-process*"
  "Just process buffer name.")

(defun just--is-recipe-line (str)
  "Is it a recipe line"
  (if (string-match "\\`[ \t\n\r]+" str)
      nil
    (s-contains? ":" str)))

(defun just--append-to-process-buffer (str)
  "Append string STR to the process buffer."
  (with-current-buffer (get-buffer-create just--process-buffer)
    (read-only-mode -1)
    (goto-char (point-max))
    (insert (format "%s\n" str))))

(defun just--find-justfiles (dir)
  "Find justfiles and returns a list of them"
  (f-files dir (lambda (file)
                 (or
                  (cl-equalp "justfile" (f-filename file))
                  (cl-equalp ".justfile" (f-filename file))))
           t))

(defun just--get-recipe-name (str)
  "Get the recipe name"
  (if (s-contains? " " str)
      (car (split-string str " "))
    str))

(defun just--arg-to-jarg (str)
  "Convert argument to jarg"
  (let* ((arg (s-split "=" str)))
    (make-jarg :arg (nth 0 arg) :default (nth 1 arg))))

(defun just--str-to-jarg (str)
  "Convert string to jarg. The string after the recipe name and
before the build constraints is expected."
  (if (and (not (s-blank? str)) str)
      (let* ((args (s-split " " str)))
        (map 'list 'just--arg-to-jarg args))
      nil
        ))

(defun just--parse-recipe (str)
  "Analyze a single recipe"
  (let*
      ((recipe-list (s-split ":" str))
       (recipe-command (just--get-recipe-name (nth 0 recipe-list)))
       (args-str (string-join (cdr (s-split " " (nth 0 recipe-list))) " "))
       (recipe-jargs (just--str-to-jarg args-str))
       )
    (make-jrecipe :name recipe-command :args recipe-jargs)
      ))

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
                  :filter 'just--xterm-color-filter
                  :sentinel #'just--sentinel
                  :file-handler t
                  :stderr nil
                  :command cmd)
    (pop-to-buffer buffer-name)))

(defun just--xterm-color-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert (xterm-color-filter string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))))))

(defun just--exec-to-string (cmd)
  "Replace \"shell-command-to-string\" to log to process buffer.

CMD is the command string to run."
  (just--log-command "just-command" cmd)
  (shell-command-to-string cmd))

(defun just--get-recipies ()
  "Get all the recipies"
  (let ((recipies (split-string (just--exec-to-string
                                 (format "just --summary --unsorted")))))
    (map 'list 'string-trim-right recipies)))

(defun just--get-jrecipies ()
  "Get list of jrecipes"
  (let ((recipies (just--get-recipies))
        )
    (map 'make-jrecipe recipies))
)

(defun just--list-to-jrecipe (list)
  "Convert list to jrecipe"
  (make-jrecipe :name (nth 0 list) :args (nth 1 list)))

(defun just-exec-recipie ()
  "Set the namespace."
  (interactive)
  (let* ((recipies (completing-read "Recipies: " (just--get-recipies)
                                     nil nil nil nil "default")))
    (just--exec "just" (list recipies))
    ))

(defvar justl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; global
    (define-key map (kbd "l") 'just-list-recipies)
    (define-key map (kbd "g") 'justl)
    (define-key map (kbd "e") 'justl-exec-recipe)
    (define-key map (kbd "?") 'justl-help-popup)
    (define-key map (kbd "h") 'justl-help-popup)
    map)
  "Keymap for `justl-mode'.")

(defun just--buffer-name ()
  "Return kubel buffer name."
  (format "*just [%s]" default-directory))

(defvar justl--line-number nil
  "Store the current line number to jump back after a refresh.")

(defun justl--save-line ()
  "Save the current line number if the view is unchanged."
  (if (equal (buffer-name (current-buffer))
             (just--buffer-name))
      (setq justl--line-number (+ 1 (count-lines 1 (point))))
    (setq justl--line-number nil)))

(defun justl--tabulated-entries (recipies)
  "Turn to tabulated entries"
  (map 'list (lambda (x) (list nil (vector x))) recipies))

(define-transient-command justl-help-popup ()
  "Justl Menu"
  [["Arguments"
    ("-d" "Dry run" "--dry-run")]
   ["Actions"
    ;; global
    ("g" "Refresh" justl)
    ("e" "Exec" justl-exec-recipe)]
   ])

(defun justl--get-recipe-from-file (filename recipe)
  (let* ((jcontent (f-read-text filename))
    (recipe-lines (split-string jcontent "\n"))
    (all-recipe (seq-filter 'just--is-recipe-line recipe-lines))
    (current-recipe (seq-filter (lambda (x) (s-contains? recipe x)) all-recipe)))
    (just--parse-recipe (car current-recipe))
    ))

(defun justl-exec-recipe (&optional args)
  "Execute just recipe.

ARGS is the arguments lit from transient"
  (interactive)
  (let* ((recipe (justl--get-word-under-cursor))
         (justfile (just--find-justfiles default-directory))
         (just-recipe (justl--get-recipe-from-file (car justfile) recipe))
         (t-args (transient-args 'justl-help-popup))
         (recipe-has-args (just--jrecipe-has-args just-recipe)))
    (if recipe-has-args
        (let* ((cmd-args (just--jrecipe-get-args just-recipe))
               (user-args (read-from-minibuffer "Just args: " (string-join cmd-args " ")))
               )
          (just--exec "just" (append t-args  (cons (jrecipe-name just-recipe) (split-string user-args " ")))))
      (just--exec "just" (append t-args (list recipe))))))

(defun justl--get-word-under-cursor ()
  "Utility function to get the name of the recipe under the cursor."
  (replace-regexp-in-string
   "^" "" (aref (tabulated-list-get-entry) 0)))

(defun justl--jump-back-to-line ()
  "Jump back to the last cached line number."
  (when justl--line-number
    (goto-line justl--line-number)))

;;;###autoload
(defun justl ()
  "Invoke the justl buffer."
  (interactive)
  (justl--save-line)
  (justl--pop-to-buffer (just--buffer-name))
  (justl-mode))

(define-derived-mode justl-mode tabulated-list-mode  "Justl"
  "Special mode for justl buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Justl")
  (setq major-mode 'justl-mode)
  (use-local-map justl-mode-map)
  (let ((justfiles (just--find-justfiles default-directory))
        (entries (just--get-recipies)))
    (message (format "test %s" entries))
    (message (format "test2  %s" (justl--tabulated-entries entries)))
    (if (null justfiles)
        (message "No justfiles found")
      (progn
        (setq tabulated-list-format [("RECIPIES" 10 t)])
        (setq tabulated-list-entries (justl--tabulated-entries entries))
        (setq tabulated-list-sort-key justl--list-sort-key)
        (setq tabulated-list-sort-key nil)
        (tabulated-list-init-header)
        (tabulated-list-print t)
        (hl-line-mode 1)
        (message (concat "Just: " default-directory))
        (run-mode-hooks 'kubel-mode-hook)))))

(add-hook 'justl-mode-hook #'justl--jump-back-to-line)

(defconst justl--list-sort-key
  '("Recipies" . nil)
  "Sort table on this key.")


(provide 'justl)
