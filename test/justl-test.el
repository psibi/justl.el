;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'justl)
(require 'ert)
(require 'f)

(ert-deftest justl--get-recipes-test ()
  (should (equal
           (list "default" "build-cmd"  "plan" "push" "push2" "fail" "carriage-return" "color" "recipe")
           (mapcar 'justl--recipe-name (justl--get-recipes "./justfile")))))

(ert-deftest justl--finds-recipe-test ()
  (let* ((recipes (justl--get-recipes "./justfile"))
	 (recipe (justl--find-recipes recipes "default")))
    (should (string= (recipe-doc recipe) "List all recipes"))))

(ert-deftest justl--get-description-test ()
  (let* ((recipes (justl--get-recipes "./justfile"))
         (recipe (seq-find (lambda (r) (string= "default" (justl--recipe-name r))) recipes)))
    (should (equal "List all recipes" (justl--recipe-desc recipe)))))

(ert-deftest justl--get-recipe-arguments-test ()
  (let* ((recipes (justl--get-recipes "./justfile"))
         (push2 (seq-find (lambda (r) (string= "build-cmd" (justl--recipe-name r))) recipes))
         (args (justl--recipe-args push2)))
    (should (equal 1 (length args)))
    (should (equal
             '("version". "0.4")
             (cons (justl--arg-name (car args)) (justl--arg-default (car args)))))))

(ert-deftest justl--lists-recipe ()
  (justl)
  (with-current-buffer (justl--buffer-name nil)
    (let ((buf-string (buffer-string)))
      (should (search-forward "plan" nil t))))
  (kill-buffer (justl--buffer-name nil)))

(defun justl--wait-till-exit (buffer)
   "Wait till the BUFFER has exited."
   (let* ((proc (get-buffer-process buffer)))
     (while (not (eq (process-status proc) 'exit))
       (sit-for 0.2))))

(ert-deftest justl--execute-recipe ()
  (justl)
  (with-current-buffer (justl--buffer-name nil)
    (search-forward "plan")
    (justl-exec-recipe)
    (justl--wait-till-exit (justl--recipe-output-buffer "plan")))
  (with-current-buffer (justl--recipe-output-buffer "plan")
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "planner" buf-string))))
  (kill-buffer (justl--buffer-name nil))
  (kill-buffer (justl--recipe-output-buffer "plan")))

(ert-deftest justl--execute-test-exit-status ()
  (justl)
  (with-current-buffer (justl--buffer-name nil)
    (search-forward "plan")
    (justl-exec-recipe)
    (justl--wait-till-exit (justl--recipe-output-buffer "plan")))
  (with-current-buffer (justl--recipe-output-buffer "plan")
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "Target execution finished" buf-string))))
  (kill-buffer (justl--buffer-name nil))
  (kill-buffer (justl--recipe-output-buffer "plan")))

(ert-deftest justl--fail-recipe ()
  (justl)
  (with-current-buffer (justl--buffer-name nil)
    (search-forward "fail")
    (justl-exec-recipe)
    (justl--wait-till-exit (justl--recipe-output-buffer "fail"))
  (with-current-buffer (justl--recipe-output-buffer "fail")
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "exited abnormally" buf-string))))
  (kill-buffer (justl--buffer-name nil))
  (kill-buffer (justl--recipe-output-buffer "fail"))))

(ert-deftest justl--find-justfile-check ()
  (should (equal (f-filename (justl--find-justfile default-directory)) "justfile")))

(ert-deftest justl--execute-recipe-which-prints-carriage-return ()
  "Carriage return should be handled in a way that allows overwriting lines."
  (justl)
  (with-current-buffer (justl--buffer-name nil)
    (search-forward "carriage-return")
    (justl-exec-recipe)
    (justl--wait-till-exit (justl--recipe-output-buffer "carriage-return")))
  (with-current-buffer (justl--recipe-output-buffer "carriage-return")
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "DONE\n" buf-string))
      (should-not (s-contains? "1/3\r2/3\r3/3\rDONE\n" buf-string))))
  (kill-buffer (justl--buffer-name nil))
  (kill-buffer (justl--recipe-output-buffer "carriage-return")))

(ert-deftest justl--execute-recipe-with-color ()
  "A target printing color is handled properly."
  (justl)
  (with-current-buffer (justl--buffer-name nil)
    (search-forward "color")
    (justl-exec-recipe)
    (justl--wait-till-exit (justl--recipe-output-buffer "color")))
  (with-current-buffer (justl--recipe-output-buffer "color")
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "This is red text\n" buf-string))))
  (kill-buffer (justl--buffer-name nil))
  (kill-buffer (justl--recipe-output-buffer "color")))

(ert-deftest justl--execute-default-recipe ()
  "Checks that default recipe is printed."
  (justl-exec-default-recipe)
  (justl--wait-till-exit justl--output-process-buffer)
  (with-current-buffer justl--output-process-buffer
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "Available recipes:\n" buf-string))))
  (kill-buffer justl--output-process-buffer))

(ert-deftest justl--execute-interactive-recipe ()
  "Checks justl-exec-recipe-in-dir indirectly (success case)."
  (justl--exec-without-justfile "just" (list "plan"))
  (justl--wait-till-exit justl--output-process-buffer)
  (with-current-buffer justl--output-process-buffer
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "planner" buf-string))))
  (kill-buffer justl--output-process-buffer))

(ert-deftest justl--execute-interactive-recipe-failure ()
  "Checks justl-exec-recipe-in-dir indrectly (failure case)."
  (justl--exec-without-justfile "just" (list "plan_non_existent"))
  (justl--wait-till-exit justl--output-process-buffer)
  (with-current-buffer justl--output-process-buffer
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "exited abnormally" buf-string))))
  (kill-buffer justl--output-process-buffer))

(ert-deftest justl--execute-interactive-recipe-multiple-args ()
  "Checks justl-exec-recipe-in-dir indrectly (failure case)."
  (justl--exec-without-justfile "just" (list "push2" "ver1" "ver2"))
  (justl--wait-till-exit justl--output-process-buffer)
  (with-current-buffer justl--output-process-buffer
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "ver1" buf-string))))
  (kill-buffer justl--output-process-buffer))

(ert-deftest justl--test-per-recipe-buffer ()
  "This test is a copy of 'justl--execute-recipe' setting the per-recipe and hardcoding the desired buffer name."
  (let ((current justl-per-recipe-buffer))
    (customize-set-variable 'justl-per-recipe-buffer 't)
    (justl)
    (with-current-buffer (justl--buffer-name nil)
      (search-forward "plan")
      (justl-exec-recipe)
      (justl--wait-till-exit "*just-plan*"))
    (with-current-buffer "*just-plan*"
      (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
        (should (s-contains? "Target execution finished" buf-string))))
    (kill-buffer (justl--buffer-name nil))
    (kill-buffer "*just-plan*")
    (customize-set-variable 'justl-per-recipe-buffer current)))

(ert-deftest justl--no-private-recipe-by-default ()
  (justl)
  (with-current-buffer (justl--buffer-name nil)
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (s-contains? "_private" buf-string)))))

(ert-deftest justl--private-recipe-visible ()
  (let ((justl-include-private-recipes t))
      (justl))
  (with-current-buffer (justl--buffer-name nil)
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "_private" buf-string)))))

(ert-deftest justl--show-modules-test ()
  "Test that justl--show-modules displays modules from a justfile with modules."
  (let* ((temp-dir (make-temp-file "justl-test" t))
         (justfile-path (expand-file-name "justfile" temp-dir))
         (module-path (expand-file-name "recipes.just" temp-dir))
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create a module file
          (write-region "# Module for testing
test-recipe:
    echo \"hello from module\"
" nil module-path)
          ;; Create main justfile that imports the module
          (write-region "mod recipes \"recipes.just\"

default:
    echo \"main recipe\"
" nil justfile-path)
          ;; Test that justl--show-modules works
          (justl--show-modules)
          (with-current-buffer (justl--buffer-name t)
            (let ((buf-string (buffer-string)))
              (should (search-forward "recipes" nil t)))))
      ;; Cleanup
      (when (get-buffer (justl--buffer-name t))
        (kill-buffer (justl--buffer-name t)))
      (delete-directory temp-dir t))))

(ert-deftest justl--modules-keybinding-test ()
  "Test that 'm' keybinding in justl buffer shows modules."
  (let* ((temp-dir (make-temp-file "justl-test" t))
         (justfile-path (expand-file-name "justfile" temp-dir))
         (module-path (expand-file-name "recipes.just" temp-dir))
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create a module file
          (write-region "# Module for testing
test-recipe:
    echo \"hello from module\"
" nil module-path)
          ;; Create main justfile that imports the module
          (write-region "#Some module descriptio
mod recipes \"recipes.just\"

default:
    echo \"main recipe\"
" nil justfile-path)
          ;; Open justl buffer
          (justl)
          (with-current-buffer (justl--buffer-name nil)
            ;; Simulate pressing 'm' key
            (call-interactively (key-binding (kbd "m")))
            ;; Check that modules buffer was created and contains module
            (with-current-buffer (justl--buffer-name t)
              (let ((buf-string (buffer-string)))
		(should (s-contains? "Some module descri" buf-string))
                (should (search-forward "recipes" nil t))
		))))
      ;; Cleanup
      (when (get-buffer (justl--buffer-name nil))
        (kill-buffer (justl--buffer-name nil)))
      (when (get-buffer (justl--buffer-name t))
        (kill-buffer (justl--buffer-name t)))
      (delete-directory temp-dir t))))

;; (ert "justl--**")

(provide 'justl-test)
;;; justl-test.el ends here
