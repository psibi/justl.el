;;; justl.el --- Major mode for driving just files -*- lexical-binding: t; -*-

;; Copyright (C) 2021, Sibi Prabakaran

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 0.13
;; Author: Sibi Prabakaran
;; Keywords: just justfile tools processes
;; URL: https://github.com/psibi/justl.el
;; License: GNU General Public License >= 3
;; Package-Requires: ((transient "0.1.0") (emacs "27.1") (s "1.2.0") (f "0.20.0") (inheritenv "0.2"))

;;; Commentary:

;; Emacs extension for driving just files
;;
;; To list all the recipes present in your justfile, call
;;
;; M-x justl
;;
;; You don't have to call it from the actual justfile.  Calling it from
;; the directory where the justfile is present should be enough.
;;
;; Alternatively, if you want to just execute a recipe, call
;;
;; M-x justl-exec-recipe-in-dir
;;
;; To execute default recipe, call justl-exec-default-recipe
;;
;; Shortcuts:
;;
;; On the just screen, place your cursor on a recipe
;;
;; h => help popup
;; ? => help popup
;; g => refresh
;; e => execute recipe
;; E => execute recipe with eshell
;; w => execute recipe with arguments
;; W => open eshell without executing
;;
;; Customize:
;;
;; By default, justl searches the executable named `just`, you can
;; change the `justl-executable` variable to set any explicit path.
;;
;; You can also control the width of the RECIPE column in the justl
;; buffer via `justl-recipe width`.  By default it has a value of 20.
;;

;;; Code:

(require 'ansi-color)
(require 'transient)
(require 'cl-lib)
(require 'eshell)
(require 'esh-mode)
(require 's)
(require 'f)
(require 'compile)
(require 'comint)
(require 'subr-x)
(require 'tramp)
(require 'inheritenv)

(defgroup justl nil
  "Justfile customization group."
  :group 'languages
  :prefix "justl-"
  :link '(url-link :tag "Site" "https://github.com/psibi/justl.el")
  :link '(url-link :tag "Repository" "https://github.com/psibi/justl.el"))

(defcustom justl-executable "just"
  "Location of just executable."
  :type 'file
  :group 'justl
  :safe 'stringp)

(defcustom justl-recipe-width 20
  "Width of the recipe column."
  :type 'integer
  :group 'justl)

(defcustom justl-justfile nil
  "Buffer local variable which points to the justfile.

If this is NIL, it means that no justfile was found.  In any
other cases, it's a known path."
  :type 'string
  :local t
  :group 'justl
  :safe 'stringp)

(defun justl--process-error-buffer (process-name)
  "Return the error buffer name for the PROCESS-NAME."
  (format "*%s:err*" process-name))

(defun justl--pop-to-buffer (name)
  "Utility function to pop to buffer or create it.

NAME is the buffer name."
  (pop-to-buffer-same-window (get-buffer-create name)))

(defvar justl--last-command nil)

(defvar justl--list-command-exit-code 0)

(defconst justl--process-buffer "*just-process*"
  "Just process buffer name.")

(defconst justl--output-process-buffer "*just*"
  "Just output process buffer name.")

(defconst justl--compilation-process-name "just-compilation-process"
  "Process name for just compilation process.")

(defun justl--append-to-process-buffer (str)
  "Append string STR to the process buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create justl--process-buffer)
      (goto-char (point-max))
      (insert (format "%s\n" str))
      (read-only-mode nil))))

(defun justl--find-any-justfile (dir)
  "Find justfile inside a sub-directory DIR or a parent directory.

Returns the absolute path if file exists or nil if no path
was found."
  (cl-flet*
      ((is-justfile (s) (string= "justfile" (downcase s)))
       (any-justfile (d) (seq-find #'is-justfile (directory-files d))))
    (when-let ((location (locate-dominating-file dir #'any-justfile)))
      (expand-file-name (any-justfile location) location))))

(defun justl--find-justfile (dir)
  "Find justfile inside a sub-directory DIR or a parent directory.

DIR represents the directory where search will be carried out.
It searches either for the filename justfile or .justfile"
  (when-let ((justfile-path (justl--find-any-justfile dir)))
    (setq-local justl-justfile justfile-path)
    justfile-path))

(defun justl--log-command (process-name cmd)
  "Log the just command to the process buffer.

PROCESS-NAME is the name of the process.
CMD is the just command as a list."
  (let ((cmd (if (listp cmd)
                 (string-join cmd " ")
               cmd)))
    (setq justl--last-command cmd)
    (justl--append-to-process-buffer
     (format "[%s] \ncommand: %S" process-name cmd))))

(defun justl--sentinel (process _)
  "Sentinel function for PROCESS."
  (let ((process-name (process-name process))
        (inhibit-read-only t)
        (exit-status (process-exit-status process)))
    (with-current-buffer (get-buffer justl--output-process-buffer)
      (goto-char (point-max))
      (if (zerop exit-status)
          (insert (format "\nTarget execution finished at %s" (substring (current-time-string) 0 19)))
        (insert (format "\nTarget execution exited abnormally with code %s at %s" exit-status (substring (current-time-string) 0 19)))))
    (unless (zerop exit-status)
      (let ((err (with-current-buffer (get-buffer-create (justl--process-error-buffer process-name))
                   (buffer-string))))
        (justl--append-to-process-buffer
         (format "[%s] error: %s"
                 process-name
                 err))))))

(defun justl--process-filter (proc string)
  "Filter function for PROC handling colors and carriage return.

STRING is the data returned by the PROC"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (widen)
        (goto-char (marker-position (process-mark proc)))
        (insert string)
        (comint-carriage-motion (process-mark proc) (point))
        (ansi-color-apply-on-region (process-mark proc) (point))
        (set-marker (process-mark proc) (point))))))

(defun justl-compilation-setup-buffer (buf dir mode &optional no-mode-line)
  "Setup the compilation buffer for just-compile-mode.

Prepare BUF for compilation process.  DIR is set as default
directory and MODE is name of the Emacs mode.  NO-MODE-LINE
controls if we are going to display the process status on mode line."
  (let ((inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (setq default-directory dir)
      (funcall mode)
      (unless no-mode-line
        (setq mode-line-process
              '((:propertize ":%s" face compilation-mode-line-run)
                compilation-mode-line-errors)))
      (force-mode-line-update)
      (if (or compilation-auto-jump-to-first-error
              (eq compilation-scroll-output 'first-error))
          (set (make-local-variable 'compilation-auto-jump-to-next) t)))))

(defvar justl--compile-command nil
  "Last shell command used to do a compilation; default for next compilation.")

(defun justl--make-process (command &optional args)
  "Start a spellcheck compilation process with COMMAND.

ARGS is a plist that affects how the process is run.
- `:no-display' don't display buffer when starting compilation process
- `:buffer' name for process buffer
- `:process' name for compilation process
- `:mode' mode for process buffer
- `:directory' set `default-directory'
- `:sentinel' process sentinel"
  (let* ((buf (get-buffer-create
               (or (plist-get args :buffer) justl--output-process-buffer)))
         (process-name (or (plist-get args :process) justl--compilation-process-name))
         (mode (or (plist-get args :mode) 'justl-compile-mode))
         (directory (or (plist-get args :directory) (f-dirname justl-justfile)))
         (sentinel (or (plist-get args :sentinel) #'justl--sentinel))
         (inhibit-read-only t))
    (setq next-error-last-buffer buf)
    (justl-compilation-setup-buffer buf directory mode)
    (with-current-buffer buf
      (insert (format "Just target execution started at %s \n\n" (substring (current-time-string) 0 19)))
      (let* ((process (apply
                       #'start-file-process process-name buf command)))
        (setq justl--compile-command command)
        (setq-local justl-justfile (justl--justfile-from-arg (elt command 1)))
        (run-hook-with-args 'compilation-start-hook process)
        (set-process-filter process 'justl--process-filter)
        (set-process-sentinel process sentinel)
        (set-process-coding-system process 'utf-8-emacs-unix 'utf-8-emacs-unix)
        (pop-to-buffer buf)))))

(defvar justl-compile-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (set-keymap-parent map compilation-mode-map)
    (define-key map [remap recompile] 'justl-recompile)
    map)
  "Keymap for justl compilation log buffers.")

(defun justl-recompile ()
  "Execute the same just target again."
  (interactive)
  (justl--make-process justl--compile-command (list :buffer justl--output-process-buffer
                                                    :process "just"
                                                    :directory (if justl-justfile
                                                                   (f-dirname justl-justfile)
                                                                 default-directory)
                                                    :mode 'justl-compile-mode)))

(defvar justl-mode-font-lock-keywords
  '(
    ("^Target execution \\(finished\\).*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 compilation-info-face))
    ("^Target execution \\(exited abnormally\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 compilation-error-face)
     (2 compilation-error-face nil t)))
  "Things to highlight in justl-compile mode.")

(define-compilation-mode justl-compile-mode "just-compile"
  "Just compilation mode.

Error matching regexes from compile.el are removed."
  (setq-local compilation-error-regexp-alist-alist nil)
  (setq-local compilation-error-regexp-alist nil)
  (setq font-lock-defaults '(justl-mode-font-lock-keywords t))
  (setq-local compilation-num-errors-found 0)
  (setq-local compilation-num-warnings-found 0)
  (setq-local compilation-num-infos-found 0)
  (setq-local overlay-arrow-string "")
  (setq next-error-overlay-arrow-position nil))

(defun justl--exec (process-name args)
  "Utility function to run commands in the proper setting.

PROCESS-NAME is an identifier for the process.  Default to \"just\".
ARGS is a ist of arguments."
  (when (equal process-name "")
    (setq process-name justl-executable))
  (let ((buffer-name justl--output-process-buffer)
        (error-buffer (justl--process-error-buffer process-name))
        (cmd (append (list justl-executable (justl--justfile-argument)) args))
        (mode 'justl-compile-mode))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (when (get-buffer error-buffer)
      (kill-buffer error-buffer))
    (justl--log-command process-name cmd)
    (justl--make-process cmd (list :buffer buffer-name
                                   :process process-name
                                   :mode mode))))

(defun justl--exec-without-justfile (process-name args)
  "Utility function to run commands in the proper setting.

PROCESS-NAME is an identifier for the process.  Default to \"just\".
ARGS is a ist of arguments."
  (when (equal process-name "")
    (setq process-name justl-executable))
  (let ((buffer-name justl--output-process-buffer)
        (error-buffer (justl--process-error-buffer process-name))
        (cmd (append (list justl-executable) args))
        (mode 'justl-compile-mode))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (when (get-buffer error-buffer)
      (kill-buffer error-buffer))
    (justl--log-command process-name cmd)
    (justl--make-process cmd (list :buffer buffer-name
                                   :process process-name
                                   :directory default-directory
                                   :mode mode)))
  (pop-to-buffer justl--output-process-buffer))

(defun justl--exec-to-string-with-exit-code (executable &rest args)
  "Run EXECUTABLE with ARGS and return (exit_code . output_string).
Logs the command run."
  (let ((cmd (string-join (cons executable (mapcar 'shell-quote-argument args)) " ")))
    (justl--log-command "just-command" cmd)
    (inheritenv
     (with-temp-buffer
       (let ((justl-status (apply 'process-file executable nil t nil args))
             (buf-string (buffer-substring-no-properties (point-min) (point-max))))
         (cons justl-status buf-string))))))

(defun justl--parse (justfile)
  "Extract info about JUSTFILE as parsed JSON."
  (let ((result (justl--exec-to-string-with-exit-code
                 justl-executable
                 (concat "--justfile=" (tramp-file-local-name justfile))
                 "--unstable" "--dump" "--dump-format=json")))
    (if (zerop (car result))
        (json-parse-string (cdr result) :null-object nil :false-object nil :array-type 'list :object-type 'alist)
      (error "Couldn't read %s: %s exited with code %d" (tramp-file-local-name justfile) justl-executable (car result)))))

(defun justl--get-recipes (justfile)
  "Return all the recipes from JUSTFILE.
They are returned as objects, as per the JSON output of \"just --dump\"."
  (let-alist (justl--parse justfile)
    (seq-sort (lambda (r1 r2) (string< (justl--recipe-name r1) (justl--recipe-name r2)))
              (mapcar 'cdr .recipes))))

(defun justl--recipe-name (recipe)
  "Get the name of RECIPE."
  (let-alist recipe .name))

(defun justl--recipe-desc (recipe)
  "Get the description of RECIPE."
  (let-alist recipe .doc))

(defun justl--recipe-args (recipe)
  "Get the arguments for RECIPE."
  (let-alist recipe .parameters))

(defun justl--arg-name (arg)
  "Get the name of argument ARG."
  (let-alist arg .name))

(defun justl--arg-default (arg)
  "Get the default value of argument ARG."
  (let-alist arg .default))

(defun justl--arg-default-as-string (arg)
  "Get the default value of argument ARG as a string.
Empty string is returned if the arg has no default."
  (if arg (format "%s" arg) ""))

(defun justl--justfile-argument ()
  "Provides justfile argument with the proper location."
  (format "--justfile=%s" (tramp-file-local-name justl-justfile)))

(defun justl--justfile-from-arg (arg)
  "Return justfile filepath from ARG."
  (when arg
    (cadr (s-split "--justfile=" arg))))

(defun justl-exec-recipe-in-dir ()
  "Populate and execute the selected recipe."
  (interactive)
  (let* ((justfile (justl--find-justfile default-directory)))
    (unless justfile
      (error "No justfile found"))
    (let* ((recipes (justl--get-recipes justfile))
           (recipe-name (completing-read "Recipes: "
                                         (mapcar 'justl--recipe-name recipes)
                                         nil t nil nil))
           (recipe (cdr (assoc recipe-name recipes))))
      (justl--exec-without-justfile
       justl-executable
       (cons recipe-name
             (mapcar (lambda (arg) (read-from-minibuffer
                                    (format "Just arg for %s: " (justl--arg-name arg))
                                    (justl--arg-default-as-string arg)))
                     (justl--recipe-args recipe)))))))

(defun justl-exec-default-recipe ()
  "Execute default recipe."
  (interactive)
  (justl--exec-without-justfile justl-executable nil))

(defvar justl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'justl--refresh-buffer)
    (define-key map (kbd "e") 'justl-exec-recipe)
    (define-key map (kbd "E") 'justl-exec-eshell)
    (define-key map (kbd "?") 'justl-help-popup)
    (define-key map (kbd "h") 'justl-help-popup)
    (define-key map (kbd "w") 'justl--exec-recipe-with-args)
    (define-key map (kbd "W") 'justl-no-exec-eshell)
    (define-key map (kbd "RET") 'justl-go-to-recipe)
    map)
  "Keymap for `justl-mode'.")

(defun justl--buffer-name ()
  "Return justl buffer name."
  (let ((justfile (justl--find-justfile default-directory)))
    (format "*just [%s]*"
            (f-dirname justfile))))

(defun justl--tabulated-entries (recipes)
  "Turn RECIPES to tabulated entries."
  (mapcar (lambda (r)
            (list
             ;; Comparison key, used to restore line number after refresh
             (justl--recipe-name r)
             (vector (propertize (justl--recipe-name r) 'recipe r)
                     (or (justl--recipe-desc r) ""))))
          recipes))

(defun justl-exec-eshell (&optional no-send)
  "Execute just recipe in eshell.
When NO-SEND is non-nil, the command is inserted ready for editing but is
not executed."
  (interactive)
  (let* ((recipe (justl--get-recipe-under-cursor))
         (eshell-buffer-name (format "justl - eshell - %s" (justl--recipe-name recipe)))
         (default-directory (f-dirname justl-justfile)))
    (eshell)
    (insert (string-join
             (cons
              justl-executable
              (cons (justl--recipe-name recipe)
                    (append (transient-args 'justl-help-popup)
                            (mapcar 'justl--arg-default-as-string
                                    (justl--recipe-args recipe)))))
             " "))
    (unless no-send
      (eshell-send-input))))

(defun justl-no-exec-eshell ()
  "Open eshell with the recipe but do not execute it."
  (interactive)
  (justl-exec-eshell t))

(transient-define-argument justl--color ()
  :description "Color output"
  :class 'transient-switches
  :key "-c"
  :argument-format "--color=%s"
  :argument-regexp "\\(--color \\(auto\\|always\\|never\\)\\)"
  :choices '("auto" "always" "never"))

(transient-define-prefix justl-help-popup ()
  "Justl Menu."
  [["Arguments"
    ("-s" "Clear shell arguments" "--clear-shell-args")
    ("-d" "Dry run" "--dry-run")
    ("-e" "Disable .env file" "--no-dotenv")
    ("-h" "Highlight" "--highlight")
    ("-n" "Disable Highlight" "--no-highlight")
    ("-q" "Quiet" "--quiet")
    ("-v" "Verbose output" "--verbose")
    ("-u" "Unstable" "--unstable")
    (justl--color)
    ]
   ["Actions"
    ;; global
    ("g" "Refresh" justl)
    ("e" "Exec" justl-exec-recipe)
    ("E" "Exec with eshell" justl-exec-eshell)
    ("w" "Exec with args" justl--exec-recipe-with-args)
    ("W" "Open eshell with args" justl-no-exec-eshell)
    ("RET" "Go to recipe" justl-go-to-recipe)
    ]
   ])

(defun justl-exec-recipe ()
  "Execute just recipe."
  (interactive)
  (let* ((recipe (justl--get-recipe-under-cursor)))
    (justl--exec
     justl-executable
     (append (transient-args 'justl-help-popup)
             (cons (justl--recipe-name recipe)
                   (mapcar (lambda (arg) (read-from-minibuffer
                                          (format "Just arg for `%s': " (justl--arg-name arg))
                                          (justl--arg-default-as-string arg)))
                           (justl--recipe-args recipe)))))))

(defun justl--exec-recipe-with-args ()
  "Execute just recipe with arguments."
  (interactive)
  (let* ((recipe (justl--get-recipe-under-cursor))
         (recipe-name (justl--recipe-name recipe))
         (user-args (read-from-minibuffer
                     (format "Arguments for `%s' separated by spaces: "
                             recipe-name))))
    (justl--exec
     justl-executable
     (append (transient-args 'justl-help-popup)
             (cons
              recipe-name
              (split-string user-args " "))))))

(defun justl-go-to-recipe ()
  "Go to the recipe on justfile."
  (interactive)
  (let* ((recipe (justl--get-recipe-under-cursor)))
    (find-file justl-justfile)
    (goto-char (point-min))
    (when (re-search-forward (concat "^[@]?" (regexp-quote (justl--recipe-name recipe)) "\\(?: .*?\\)?:") nil t 1)
      (goto-char (line-beginning-position)))))

(defun justl--get-recipe-under-cursor ()
  "Utility function to get the name of the recipe under the cursor."
  (get-text-property 0 'recipe (aref (tabulated-list-get-entry) 0)))

(defun justl--refresh-buffer ()
  "Refresh justl buffer."
  (interactive)
  (let* ((justfile (justl--find-justfile default-directory))
         (entries (justl--get-recipes justfile)))
    (setq tabulated-list-entries (justl--tabulated-entries entries))
    (tabulated-list-print t)
    (message "justl-mode: Refreshed")))

;;;###autoload
(defun justl ()
  "Invoke the justl buffer."
  (interactive)
  (unless (justl--find-justfile default-directory)
    (error "No justfile found"))
  (justl--pop-to-buffer (justl--buffer-name))
  (justl-mode)
  (justl--refresh-buffer))

(define-derived-mode justl-mode tabulated-list-mode  "Justl"
  "Special mode for justl buffers."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq tabulated-list-format
        (vector (list "RECIPES" justl-recipe-width t)
                (list "DESCRIPTION" 20 t)))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (hl-line-mode 1))

(provide 'justl)
;;; justl.el ends here
