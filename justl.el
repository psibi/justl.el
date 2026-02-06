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

;; Version: 0.15
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
;; E => execute recipe with a shell
;; w => execute recipe with arguments
;; W => open a shell without executing
;;
;; Customize:
;;
;; By default, justl searches the executable named `just`, you can
;; change the `justl-executable` variable to set any explicit path.
;;
;; You can also control the width of the RECIPE column in the justl
;; buffer via `justl-recipe width`.  By default it has a value of 20.
;;
;; You can change the shell between `eshell' and `vterm' using the
;; `justl-shell' variable.  Using vterm requires the `vterm' package to
;; be installed.
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
  :group 'justl
  :type 'integer)

(defcustom justl-justfile nil
  "Buffer local variable which points to the justfile.

If this is NIL, it means that no justfile was found.  In any
other cases, it's a known path."
  :type 'string
  :group 'justl
  :local t
  :safe 'stringp)

(defcustom justl-include-private-recipes nil
  "If non-nil, include private recipes in the list."
  :type 'boolean
  :group 'justl
  :safe 'booleanp)

(defcustom justl-per-recipe-buffer nil
  "If non-nil, create a new buffer per recipe."
  :type 'boolean
  :group 'justl
  :safe 'booleanp)

(defcustom justl-shell 'eshell
  "Shell to use when running recipes.
Can be either Eshell, vterm, or eat.  Using vterm requires the vterm
package to be installed.  Using eat requires the eat package to be installed."

  :type '(choice (const eshell)
                 (const vterm)
                 (const eat))
  :group 'justl)

(defcustom justl-pop-to-buffer-on-display t
  "If non-nil, selects the justl output buffer when it is displayed.
If nil, displays the output buffer without selecting it."
  :type 'boolean
  :group 'justl
  :safe 'booleanp)

(defun justl--recipe-output-buffer (recipe-name)
  "Return the buffer name for the RECIPE-NAME."
  (if justl-per-recipe-buffer
      (format "*just-%s*" recipe-name)
    justl--output-process-buffer))

(defun justl--process-error-buffer (process-name)
  "Return the error buffer name for the PROCESS-NAME."
  (format "*%s:err*" process-name))

(defun justl--pop-to-buffer (name)
  "Utility function to pop to buffer or create it.

NAME is the buffer name."
  (pop-to-buffer-same-window (get-buffer-create name)))

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
      ((is-justfile (s) (s-ends-with? "justfile" s t))
       (any-justfile (d) (seq-find #'is-justfile (directory-files d))))
    (when-let ((location (locate-dominating-file dir #'any-justfile)))
      (expand-file-name (any-justfile location) location))))

(defun justl--find-justfile (dir)
  "Find justfile inside a sub-directory DIR or a parent directory.

DIR represents the directory where search will be carried out.
It searches either for the filename justfile or .justfile"
  (when-let ((justfile-path (or (justl--find-any-justfile dir) (getenv "JUST_JUSTFILE"))))
    (setq-local justl-justfile justfile-path)
    justfile-path))

(defun justl--log-command (process-name cmd)
  "Log the just command to the process buffer.

PROCESS-NAME is the name of the process.
CMD is the just command as a list."
  (let ((cmd (if (listp cmd)
                 (string-join cmd " ")
               cmd)))
    (justl--append-to-process-buffer
     (format "[%s] \ncommand: %S" process-name cmd))))

(defun justl--sentinel (process buffer)
  "Sentinel function for PROCESS.

BUFFER is the name of the output buffer."
  (let ((process-name (process-name process))
        (inhibit-read-only t)
        (exit-status (process-exit-status process)))
    (with-current-buffer buffer
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

(defvar-local justl--compile-command nil
  "Last shell command used to do a compilation; default for next compilation.")

(defun justl--make-process (command &optional args)
  "Start a spellcheck compilation process with COMMAND.

ARGS is a plist that affects how the process is run.
- `:no-display' don't display buffer when starting compilation process
- `:buffer' name for process buffer
- `:process' name for compilation process
- `:mode' mode for process buffer
- `:directory' set `default-directory'"
  (let* ((buf (get-buffer-create
               (or (plist-get args :buffer) justl--output-process-buffer)))
         (process-name (or (plist-get args :process) justl--compilation-process-name))
         (mode (or (plist-get args :mode) 'justl-compile-mode))
         (directory (or (plist-get args :directory) (f-dirname justl-justfile)))
         (inhibit-read-only t))
    (setq next-error-last-buffer buf)
    (justl-compilation-setup-buffer buf directory mode)
    (with-current-buffer buf
      (insert (format "Just target execution started at %s \n\n" (substring (current-time-string) 0 19)))
      (let* ((default-directory directory)
	     (process (apply
                       #'start-file-process process-name buf command)))
        (setq justl--compile-command command)
        (setq-local justl-justfile (justl--justfile-from-arg (elt command 1)))
        (run-hook-with-args 'compilation-start-hook process)
        (set-process-filter process 'justl--process-filter)
        (set-process-sentinel process (lambda (proc _) (justl--sentinel proc buf)))
        (set-process-coding-system process 'utf-8-emacs-unix 'utf-8-emacs-unix)
        (if justl-pop-to-buffer-on-display
            (pop-to-buffer buf)
          (display-buffer buf))))))

(defvar justl-compile-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (set-keymap-parent map compilation-mode-map)
    (define-key map [remap recompile] 'justl-recompile)
    map)
  "Keymap for justl compilation log buffers.")

(defun justl-set-new-working-dir (dir)
  "Set DIR as the new working directory.
This is usually used with no-cd recipe attribute."
  (interactive "DNew Working Directory:")
  (setq-local default-directory dir))

(defun justl-recompile ()
  "Execute the same just target again."
  (interactive)
  ;; This is copied and adapted from `compilation-start'.
  (let ((comp-proc (get-buffer-process (current-buffer))))
    (if comp-proc
        (if (or (not (eq (process-status comp-proc) 'run))
                (eq (process-query-on-exit-flag comp-proc) nil)
                (yes-or-no-p "The last target is still running; kill it? "))
            (condition-case ()
                (progn
                  (interrupt-process comp-proc)
                  (sit-for 1)
                  (delete-process comp-proc))
              (error nil))
          (error "Cannot have two processes in `%s' at once"
                 (buffer-name)))))

  (justl--make-process justl--compile-command (list :buffer (buffer-name)
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

(defun justl--exec (process-name recipe-name args)
  "Utility function to run commands in the proper setting.

PROCESS-NAME is an identifier for the process.  Default to \"just\".
RECIPE-NAME is the name of the recipe.
ARGS is a list of arguments."
  (when (equal process-name "")
    (setq process-name justl-executable))
  (let ((buffer-name (justl--recipe-output-buffer recipe-name))
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
				   :directory default-directory
                                   :mode mode))))

(defun justl--exec-without-justfile (process-name args buffer-name)
  "Utility function to run commands in the proper setting.

PROCESS-NAME is an identifier for the process.  Default to \"just\".
ARGS is a ist of arguments.
BUFFER-NAME is the output buffer name."
  (when (equal process-name "")
    (setq process-name justl-executable))
  (let ((error-buffer (justl--process-error-buffer process-name))
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
  (if justl-pop-to-buffer-on-display
      (pop-to-buffer buffer-name)
    (display-buffer buffer-name)))

(defun justl--exec-to-string-with-exit-code (executable &rest args)
  "Run EXECUTABLE with ARGS, throwing an error if the command fails.
Logs the command run."
  (let ((cmd (string-join (cons executable (mapcar 'shell-quote-argument args)) " ")))
    (justl--log-command "just-command" cmd)
    (inheritenv
     (with-temp-buffer
       (let ((exit-code (apply 'process-file executable nil t nil args)))
         (if (zerop exit-code)
             (buffer-substring-no-properties (point-min) (point-max))
           (error "Command failed with exit code %d: %S" exit-code (cons executable args))))))))

(defun justl--parse (justfile)
  "Extract info about JUSTFILE as parsed JSON."
  (let* ((base-args (append `(,justl-executable)
                             (transient-args 'justl-help-popup)
                             `(,(justl--justfile-argument justfile))))
         (json (apply 'justl--exec-to-string-with-exit-code
                      (delete-dups (append base-args '("--unstable" "--dump" "--dump-format=json")))))
        ;; Obtain the unsorted declaration order separately
        (unsorted-recipes (s-split
                           " "
                           (s-trim-right
                            (apply 'justl--exec-to-string-with-exit-code
                                   (append base-args '("--summary" "--unsorted"))))
                           t)))
    (let ((parsed (json-parse-string json :null-object nil :false-object nil :array-type 'list :object-type 'alist)))
      (cl-flet ((unsorted-index (r)
                  (let-alist (cdr r)
                    (or (cl-position .name unsorted-recipes :test 'string=)
                        ;; sort private commands last
                        1000))))
        (let ((recipes-entry (assoc 'recipes parsed)))
          (setcdr recipes-entry
                  (seq-sort (lambda (a b) (< (unsorted-index a) (unsorted-index b)))
                            (cdr recipes-entry)))
          parsed)))))

(cl-defstruct recipe
  ;; Recipe name
  name
  ;; Optional recipe documentation
  doc
  ;; Parameters for the recipe
  parameters
  ;; Is the recipe private
  private)

(defun justl--get-recipes (justfile)
  "Return all the recipes from JUSTFILE.
They are returned as objects, as per the JSON output of \"just --dump\"."
  (let-alist (justl--parse justfile)
    (let ((all-recipes (mapcar (lambda (x) (make-recipe :name (alist-get 'name x)
				     :doc (alist-get 'doc x)
				     :parameters (alist-get 'parameters x)
				     :private (alist-get 'private x))) .recipes)))
      (if justl-include-private-recipes
	  all-recipes
	(seq-filter (lambda (recipe) (not (justl--recipe-private-p recipe))) all-recipes)))))

;;; todo: For easily integrating it, we need something like this
;;; integrated upstream:
;;; https://github.com/casey/just/issues/2252#issuecomment-2474171211
(defun justl--get-modules (justfile)
  "Return all the modules from JUSTFILE.
They are returned as objects, as per the JSON output of \"just --dump\"."
  (let-alist (justl--parse justfile)
    (mapcar 'car .modules)))

(defun justl--recipe-name (recipe)
  "Get the name of RECIPE."
  (recipe-name recipe))

(defun justl--recipe-desc (recipe)
  "Get the description of RECIPE."
  (recipe-doc recipe))

(defun justl--recipe-args (recipe)
  "Get the arguments for RECIPE."
  (recipe-parameters recipe))

(defun justl--recipe-private-p (recipe)
  "Return non-nil if RECIPE is private."
  (recipe-private recipe))

(defun justl--arg-name (arg)
  "Get the name of argument ARG."
  (let-alist arg .name))

(defun justl--arg-default (arg)
  "Get the default value of argument ARG."
  (let-alist arg .default))

(defun justl--justfile-argument (&optional justfile)
  "Provides JUSTFILE argument with the proper location."
  (format "--justfile=%s" (tramp-file-local-name (or justfile justl-justfile))))

(defun justl--justfile-from-arg (arg)
  "Return justfile filepath from ARG."
  (when arg
    (cadr (s-split "--justfile=" arg))))

(defvar-local justl--recipes nil
  "Set of recipes loaded for `justl-exec-recipe-in-dir'")

(defun justl-completion-annotation (candidate)
  "Annotation function for `justl-exec-recipe-in-dir'."
  (let* ((recipes (buffer-local-value 'justl--recipes (window-buffer (minibuffer-selected-window))))
         (doc (cl-some
               (lambda (recipe)
                 (when (string= (justl--recipe-name recipe) candidate)
                   (justl--recipe-desc recipe)))
               recipes)))
    (when doc
      (concat  (propertize (concat " -- " doc) 'face 'font-lock-comment-face)))))

;;;###autoload
(defun justl-exec-recipe-in-dir ()
  "Populate and execute the selected recipe."
  (interactive)
  (let* ((justfile (justl--find-justfile default-directory)))
    (unless justfile
      (error "No justfile found"))
    (setq justl--recipes (justl--get-recipes justfile))
    (let* ((recipes justl--recipes)
           (recipe-names (mapcar 'justl--recipe-name justl--recipes))
           (recipe-name (completing-read "Recipe: "
                                         (lambda (string pred action)
                                           (if (eq action 'metadata)
                                               '(metadata (annotation-function . justl-completion-annotation)
                                                          (category . just-recipe))
                                             (complete-with-action action recipe-names string pred)))
                                         nil t nil nil))
           (recipe (justl--find-recipes recipes recipe-name)))
      (justl--exec-without-justfile
       justl-executable
       (cons recipe-name
             (mapcar 'justl--read-arg
                     (justl--recipe-args recipe)))
       (justl--recipe-output-buffer recipe-name)))))

;;;###autoload
(defun justl-exec-default-recipe ()
  "Execute default recipe."
  (interactive)
  (justl--exec-without-justfile justl-executable nil (justl--recipe-output-buffer "default")))

(defvar justl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'justl--refresh-buffer)
    (define-key map (kbd "e") 'justl-exec-recipe)
    (define-key map (kbd "E") 'justl-exec-shell)
    (define-key map (kbd "?") 'justl-help-popup)
    (define-key map (kbd "h") 'justl-help-popup)
    (define-key map (kbd "w") 'justl--exec-recipe-with-args)
    (define-key map (kbd "W") 'justl-no-exec-shell)
    (define-key map (kbd "m")  'justl--show-modules)
    (define-key map (kbd "RET") 'justl-go-to-recipe)
    map)
  "Keymap for `justl-mode'.")

(defun justl--buffer-name (is-module)
  "Return justl buffer name."
  (let ((justfile (justl--find-justfile default-directory)))
    (format "*just [%s] %s*" justfile (if is-module "(Module)" ""))))

(defun justl--tabulated-entries (recipes)
  "Turn RECIPES to tabulated entries."
  (mapcar (lambda (r)
            (list
             ;; Comparison key, used to restore line number after refresh
             (justl--recipe-name r)
             (vector (propertize (justl--recipe-name r) 'recipe r)
                     (or (justl--recipe-desc r) ""))))
          recipes))

(defun justl--tabulated-modules-entries (modules)
  "Turn MODULES to tabulated entries."
  (mapcar (lambda (m)
            (list
             ;; Comparison key, used to restore line number after refresh
             (just-module-name m)
             (vector (propertize (just-module-name m) 'module m)
                     (or (just-module-doc m) ""))))
          modules))

(defun justl-exec-eshell (&optional no-send)
  "Execute just recipe in eshell.
When NO-SEND is non-nil, the command is inserted ready for editing but is
not executed."
  (interactive)
  (let* ((recipe (justl--get-recipe-under-cursor))
         (eshell-buffer-name (format "justl - eshell - %s" (justl--recipe-name recipe)))
         (default-directory (f-dirname justl-justfile)))
    (eshell)

    (let* ((recipe-name (justl--recipe-name recipe))
           (recipe-args (justl--recipe-args recipe))
           (transient-args (transient-args 'justl-help-popup))
           (args-list (cons justl-executable
                            (append transient-args
                                    (list recipe-name)
                                    (mapcar 'justl--arg-default recipe-args)))))
      (insert (string-join args-list " ")))
    (unless no-send
      (eshell-send-input))))

(defun justl-no-exec-eshell ()
  "Open eshell with the recipe but do not execute it."
  (interactive)
  (justl-exec-eshell t))

(defun justl-exec-vterm (&optional no-send)
  "Execute just recipe in vterm.
When NO-SEND is non-nil, the command is inserted ready for editing but
is not executed."
  (interactive)
  (unless (require 'vterm nil t)
    (user-error "Package `vterm' was not found!"))
  (let* ((recipe (justl--get-recipe-under-cursor))
         (vterm-buffer-name (format "justl - vterm - %s" (justl--recipe-name recipe)))
         (default-directory (f-dirname justl-justfile)))
    (vterm)

    (let* ((recipe-name (justl--recipe-name recipe))
           (recipe-args (justl--recipe-args recipe))
           (transient-args (transient-args 'justl-help-popup))
           (args-list (append (list "exec" justl-executable)
                              transient-args
                              (list recipe-name)
                              (mapcar 'justl--arg-default recipe-args))))
      (vterm-insert (string-join args-list " ")))
    (unless no-send
      (vterm-send-return))))

(defun justl-no-exec-vterm ()
  "Open vterm with the recipe but do not execute it."
  (interactive)
  (justl-exec-vterm t))

(defun justl-exec-eat (&optional no-send)
  "Execute just recipe in eat.
When NO-SEND is non-nil, the command is inserted ready for editing but
is not executed."
  (interactive)
  (unless (require 'eat nil t)
    (user-error "Package `eat' was not found!"))
  (let* ((recipe (justl--get-recipe-under-cursor))
         (eat-buffer-name (format "justl - eat - %s" (justl--recipe-name recipe)))
         (default-directory (f-dirname justl-justfile)))
    (eat)
    (let ((eat-buffer (eat eat-buffer-name)))
      (with-current-buffer eat-buffer
        (let* ((recipe-name (justl--recipe-name recipe))
               (recipe-args (justl--recipe-args recipe))
               (transient-args (transient-args 'justl-help-popup))
               (args-list (cons justl-executable
                                (append transient-args
                                        (list recipe-name)
                                        (mapcar 'justl--arg-default recipe-args)))))
          (let ((command-string (string-join args-list " ")))
            (eat-term-send-string eat-terminal command-string))
          (unless no-send
            (eat-term-send-string eat-terminal "\r")))))))

(defun justl-no-exec-eat ()
  "Open eat with the recipe but do not execute it."
  (interactive)
  (justl-exec-eat t))

(defun justl-exec-shell (&optional no-send)
  "Execute just recipe in `justl-shell'.
When NO-SEND is non-nil, the command is inserted ready for editing but
is not executed."
  (interactive)
  (pcase justl-shell
    ('eshell (justl-exec-eshell no-send))
    ('vterm (justl-exec-vterm no-send))
    ('eat (justl-exec-eat no-send))
    (_ (user-error "Invalid value for `justl-shell'"))))

(defun justl-no-exec-shell ()
  "Open `justl-shell' with the recipe but do not execute it."
  (interactive)
  (justl-exec-shell t))

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
    ("E" "Exec with shell" justl-exec-shell)
    ("w" "Exec with args" justl--exec-recipe-with-args)
    ("W" "Open shell with args" justl-no-exec-shell)
    ("m" "Show modules" justl--show-modules)
    ("RET" "Go to recipe" justl-go-to-recipe)
    ]
   ])

(defun justl--show-modules ()
  "Open modules buffer."
  (interactive)
  (justl-module))

(defun justl--read-arg (arg)
  "Read a value for ARG from the minibuffer."
  (let ((default (justl--arg-default arg)))
    (read-from-minibuffer
     (format "Just arg for '%s': " (justl--arg-name arg))
     (pcase default
       (`("variable" ,name) name)
       ('nil nil)
       (_ default)))))

(defun justl--find-recipes (recipes name)
  "Return recipe from RECIPES matching NAME."
  (seq-first (seq-filter (lambda (recipe) (string= (recipe-name recipe) name)) recipes)))

(defun justl-exec-recipe ()
  "Execute just recipe."
  (interactive)
  (let* ((recipe (justl--get-recipe-under-cursor)))
    (justl--exec
     justl-executable
     (justl--recipe-name recipe)
     (append (transient-args 'justl-help-popup)
             (cons (justl--recipe-name recipe)
                   (mapcar 'justl--read-arg
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
     recipe-name			; Buffer name
     (append (transient-args 'justl-help-popup)
	     (list recipe-name)
	     (split-string user-args " ")))))

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
  (let ((entry (tabulated-list-get-entry)))
    (if entry
        (get-text-property 0 'recipe (aref entry 0))
      (user-error "There is no recipe on the current line"))))

(defun justl--get-module-under-cursor ()
  "Utility function to get the name of the module under the cursor."
  (let ((entry (tabulated-list-get-entry)))
    (if entry
        (get-text-property 0 'module (aref entry 0))
      (user-error "There is no module on the current line"))))

(defun justl--refresh-buffer ()
  "Refresh justl buffer."
  (interactive)
  (let* ((justfile (justl--find-justfile default-directory))
         (entries (justl--get-recipes justfile)))
    (setq tabulated-list-entries (justl--tabulated-entries entries))
    (tabulated-list-print t)))

(cl-defstruct just-module
  ;; Module name
  name
  ;; Optional module documentation
  doc
  ;; Source path of the module
  source
  )

(defun justl--module-refresh-buffer ()
  "Refresh justl module buffer."
  (interactive)
  (let* ((justfile (justl--find-justfile default-directory))
         (modules (justl--get-modules justfile)))
    (setq tabulated-list-entries (justl--tabulated-modules-entries modules))
    (tabulated-list-print t)
    (message "justl-module-mode: Refreshed")))

;;; todo: For easily integrating it, we need something like this
;;; integrated upstream:
;;; https://github.com/casey/just/issues/2252#issuecomment-2474171211
(defun justl--get-modules (justfile)
  "Return all the modules from JUSTFILE.
They are returned as objects, as per the JSON output of \"just --dump\"."
  (let-alist (justl--parse justfile)
    (mapcar (lambda (x) (make-just-module :name (symbol-name (car x))
					  :doc (alist-get 'doc (cdr x))
					  :source (alist-get 'source (cdr x)))) .modules)))

;;;###autoload
(defun justl ()
  "Invoke the justl buffer."
  (interactive)
  (unless (justl--find-justfile default-directory)
    (error "No justfile found"))
  (justl--pop-to-buffer (justl--buffer-name nil))
  (justl-mode)
  (justl--refresh-buffer))

(define-derived-mode justl-mode tabulated-list-mode  "Justl"
  "Special mode for justl buffers."
  :group 'justl
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq tabulated-list-format
        (vector (list "RECIPES" justl-recipe-width t)
                (list "DESCRIPTION" 20 t)))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (hl-line-mode 1))

;;;###autoload
(defun justl-module ()
  "Invoke the justl-module buffer."
  (interactive)
  (unless (justl--find-justfile default-directory)
    (error "No justfile found"))
  (justl--pop-to-buffer (justl--buffer-name t))
  (justl-module-mode)
  (justl--module-refresh-buffer))

(defvar justl-module-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'justl--module-refresh-buffer)
    (define-key map (kbd "e") 'justl-exec-module)
    (define-key map (kbd "?") 'justl-module-help-popup)
    (define-key map (kbd "h") 'justl-module-help-popup)
    (define-key map (kbd "o") 'justl--module-open-justl)
    (define-key map (kbd "RET") 'justl--go-to-module)
    map)
  "Keymap for `justl-module-mode'.")

(define-derived-mode justl-module-mode tabulated-list-mode  "Justl (M)"
  "Special mode for justl module buffers."
  :group 'justl
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq tabulated-list-format
        (vector (list "MODULES" justl-recipe-width t)
                (list "DESCRIPTION" 20 t)))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (hl-line-mode 1))

(transient-define-prefix justl-module-help-popup ()
  "Justl Module Menu."
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
    ("g" "Refresh" justl-module)
    ("e" "Exec" justl-exec-module)
    ("o" "Open justl buffer for the module" justl--module-open-justl)
    ("RET" "Go to module" justl--go-to-module)
    ]
   ])

(defun justl-exec-module ()
  "Execute first recipe of the module."
  (interactive)
  (let* ((module (justl--get-module-under-cursor))
	 (default-directory (f-dirname (just-module-source module))))
    (justl--exec
     justl-executable
     (just-module-name module)
     (append (transient-args 'justl-module-help-popup) (cons (just-module-name module) nil)))))

(defun justl--go-to-module ()
  "Go to the module path."
  (interactive)
  (let* ((module (justl--get-module-under-cursor)))
    (find-file (just-module-source module))
    (goto-char (point-min))))

(defun justl--module-open-justl ()
  "Open justl buffer for that specific module."
  (interactive)
  (let* ((module (justl--get-module-under-cursor))
	 (default-directory (f-dirname (just-module-source module))))
    (justl)))

(provide 'justl)
;;; justl.el ends here
