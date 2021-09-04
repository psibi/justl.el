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

(setq test "hell")

(bound-and-true-p nil)

(defun just-runner (path args)
  (let ((justfile-arg (if (bound-and-true-p path)
                          (list "--justfile" path)
                        nil))))
  (call-process just-executable))

(defun just-list-entries (path)
  (just-executable)
  )
