;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'justl)
(require 'ert)
(require 'f)

(ert-deftest justl--get-recipes-test ()
  (should (equal
           (list "default" "build-cmd" "plan" "push" "push2" "fail" "carriage-return"
                 "color")
           (justl--get-recipes "./justfile"))))

(ert-deftest justl--list-to-recipe-test ()
  (should (equal
           (justl-jrecipe-name (justl--list-to-jrecipe (list "recipe" "arg")))
           "recipe"))
  (should (equal
           (justl-jrecipe-name (justl--list-to-jrecipe (list "recipe"))) "recipe"))
  (should (equal
           (justl-jrecipe-args (justl--list-to-jrecipe (list "recipe")))
           nil)))

(ert-deftest justl--recipe-has-args-test ()
  (should (equal
           (justl--jrecipe-has-args-p
            (make-justl-jrecipe
             :name "default"
             :args nil))
           nil)))

(ert-deftest justl--jrecipe-get-args-test ()
  (should (equal (justl--jrecipe-get-args
                  (make-justl-jrecipe
                   :name "default"
                   :args nil))
                 (list)))
  (should (equal (justl--jrecipe-get-args
                  (make-justl-jrecipe
                   :name "default"
                   :args (list (make-justl-jarg
                                :arg "version"
                                :default "'0.4'"))))
                 (list "version='0.4'")))
  (should (equal (justl--jrecipe-get-args
                  (make-justl-jrecipe
                   :name "default"
                   :args (list
                          (make-justl-jarg
                           :arg "version1"
                           :default nil)
                          (make-justl-jarg
                           :arg "version2"
                           :default nil))))
                 (list "version1=" "version2="))))

(ert-deftest justl--is-recipe-line-p-test ()
  (should (equal
           (justl--is-recipe-line-p "default:")
           t))
  (should (equal
           (justl--is-recipe-line-p "build-cmd version='0.4':")
           t))
  (should (equal
           (justl--is-recipe-line-p "version := 4.2")
           nil))
  (should (equal
           (justl--is-recipe-line-p "# Terraform plan")
           nil))
  (should (equal
           (justl--is-recipe-line-p "!include lorem.just")
           nil))
  (should (equal
           (justl--is-recipe-line-p "push version: (build-cmd version)")
           t))
  (should (equal
           (justl--is-recipe-line-p "    just --list")
           nil)))

(ert-deftest justl--get-recipe-from-file-test ()
  (should (equal
           (justl--get-recipe-from-file "./justfile" "default")
           (make-justl-jrecipe :name "default" :args nil)))
  (should (equal
           (justl--get-recipe-from-file "./justfile" "plan")
           (make-justl-jrecipe :name "plan" :args nil)))
  (should (equal
           (justl--get-recipe-from-file "./justfile" "push")
           (make-justl-jrecipe :name "push" :args
                               (list (make-justl-jarg :arg "version" :default nil)))))
  (should (equal
           (justl--get-recipe-from-file "./justfile" "push2")
           (make-justl-jrecipe :name "push2" :args
                               (list (make-justl-jarg :arg "version1" :default nil)
                                     (make-justl-jarg :arg "version2" :default nil))))))

(ert-deftest justl--get-recipe-name-test ()
  (should (equal
           (justl--get-recipe-name "default")
           "default"))
  (should (equal
           (justl--get-recipe-name "build-cmd version='0.4'")
           "build-cmd"))
  (should (equal
           (justl--get-recipe-name "    push version")
           "push"))
  (should (equal
           (justl--get-recipe-name "    build-cmd version='0.4' ")
           "build-cmd"))
  (should (equal
           (justl--get-recipe-name "push version:")
           "push"))
  (should (equal
           (justl--get-recipe-name "push version1 version2")
           "push")))

(ert-deftest justl--str-to-jarg-test ()
  (should (equal
           (justl--str-to-jarg "version=0.4")
           (list (make-justl-jarg :arg "version" :default "0.4"))))
  (should (equal
           (justl--str-to-jarg "version='0.4'")
           (list (make-justl-jarg :arg "version" :default "'0.4'"))))
  (should (equal
           (justl--str-to-jarg "version='0.4' version2")
           (list (make-justl-jarg :arg "version" :default "'0.4'")
                 (make-justl-jarg :arg "version2" :default nil))))
  (should (equal
           (justl--str-to-jarg "version version2")
           (list (make-justl-jarg :arg "version" :default nil)
                 (make-justl-jarg :arg "version2" :default nil))))
  (should (equal (justl--str-to-jarg "") nil)))

(ert-deftest justl--parse-recipe-test ()
  (should (equal
           (justl--parse-recipe "default:")
           (make-justl-jrecipe :name "default" :args nil)))
  (should (equal
           (justl--parse-recipe "build-cmd version='0.4':")
           (make-justl-jrecipe
            :name "build-cmd"
            :args (list
                   (make-justl-jarg
                    :arg "version"
                    :default "'0.4'")))))
  (should (equal (justl--parse-recipe "push version version2:")
                 (make-justl-jrecipe
                  :name "push"
                  :args (list (make-justl-jarg :arg "version" :default nil)
                              (make-justl-jarg :arg "version2" :default nil)))))
  (should (equal (justl--parse-recipe "push version: (build-cmd version)")
                 (make-justl-jrecipe
                  :name "push"
                  :args (list (make-justl-jarg :arg "version" :default nil))))))

(ert-deftest justl--lists-recipe ()
  (justl)
  (with-current-buffer (justl--buffer-name)
    (let ((buf-string (buffer-string)))
      (should (s-contains? "plan" buf-string))))
  (kill-buffer (justl--buffer-name)))

(defun justl--wait-till-exit (buffer)
   "Wait till the BUFFER has exited."
   (let* ((proc (get-buffer-process buffer)))
     (while (not (eq (process-status proc) 'exit))
       (sit-for 0.2))))

(ert-deftest justl--execute-recipe ()
  (justl)
  (with-current-buffer (justl--buffer-name)
    (search-forward "plan")
    (justl-exec-recipe)
    (justl--wait-till-exit justl--output-process-buffer))
  (with-current-buffer justl--output-process-buffer
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "planner" buf-string))))
  (kill-buffer (justl--buffer-name))
  (kill-buffer justl--output-process-buffer))

(ert-deftest justl--execute-test-exit-status ()
  (justl)
  (with-current-buffer (justl--buffer-name)
    (search-forward "plan")
    (justl-exec-recipe)
    (justl--wait-till-exit justl--output-process-buffer))
  (with-current-buffer justl--output-process-buffer
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "Target execution finished" buf-string))))
  (kill-buffer (justl--buffer-name))
  (kill-buffer justl--output-process-buffer))

(ert-deftest justl--fail-recipe ()
  (justl)
  (with-current-buffer (justl--buffer-name)
    (search-forward "fail")
    (justl-exec-recipe)
    (justl--wait-till-exit justl--output-process-buffer))
  (with-current-buffer justl--output-process-buffer
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "exited abnormally" buf-string))))
  (kill-buffer (justl--buffer-name))
  (kill-buffer justl--output-process-buffer))

(ert-deftest justl--find-justfiles-check ()
  (should (equal (f-filename (justl--find-justfiles default-directory)) "justfile")))

(ert-deftest justl--get-recipes-with-desc-check ()
  (let* ((justfile (justl--find-justfiles default-directory))
         (recipes (justl--get-recipes-with-desc justfile)))
    (should (member (list "default" "List all recipes") recipes))
    (should (member (list "push" nil) recipes))
    (should (member (list "push2" nil) recipes))))

(ert-deftest justl--execute-recipe-which-prints-carriage-return ()
  "Carriage return should be handled in a way that allows overwriting lines."
  (justl)
  (with-current-buffer (justl--buffer-name)
    (search-forward "carriage-return")
    (justl-exec-recipe)
    (justl--wait-till-exit justl--output-process-buffer))
  (with-current-buffer justl--output-process-buffer
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "DONE\n" buf-string))
      (should-not (s-contains? "1/3\r2/3\r3/3\rDONE\n" buf-string))))
  (kill-buffer (justl--buffer-name))
  (kill-buffer justl--output-process-buffer))

(ert-deftest justl--execute-recipe-with-color ()
  "A target printing color is handled properly."
  (justl)
  (with-current-buffer (justl--buffer-name)
    (search-forward "color")
    (justl-exec-recipe)
    (justl--wait-till-exit justl--output-process-buffer))
  (with-current-buffer justl--output-process-buffer
    (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
      (should (s-contains? "This is red text\n" buf-string))))
  (kill-buffer (justl--buffer-name))
  (kill-buffer justl--output-process-buffer))

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

;; (ert "justl--**")

(provide 'justl-test)
;;; justl-test.el ends here
