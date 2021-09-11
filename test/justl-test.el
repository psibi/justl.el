;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'justl)
(require 'ert)

(ert-deftest justl--get-recipies-test ()
  (should (equal
           (list "default" "build-cmd" "plan" "push" "push2")
           (justl--get-recipies))))

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
            (make-jrecipe
             :name "default"
             :args nil))
           nil)))

(ert-deftest justl--jrecipe-get-args-test ()
  (should (equal (justl--jrecipe-get-args
                  (make-jrecipe
                   :name "default"
                   :args nil))
                 (list)))
  (should (equal (justl--jrecipe-get-args
                  (make-jrecipe
                   :name "default"
                   :args (list (make-justl-jarg
                                :arg "version"
                                :default "'0.4'"))))
                 (list "version='0.4'")))
  (should (equal (justl--jrecipe-get-args
                  (make-jrecipe
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
           (justl--is-recipe-line-p "push version: (build-cmd version)")
           t))
  (should (equal
           (justl--is-recipe-line-p "    just --list")
           nil)))

(ert-deftest justl--find-justfiles-test ()
  (should (equal (length (justl--find-justfiles ".")) 1)))

(ert-deftest justl--get-recipe-from-file-test ()
  (should (equal
           (justl--get-recipe-from-file "./justfile" "default")
           (make-jrecipe :name "default" :args nil)))
  (should (equal
           (justl--get-recipe-from-file "./justfile" "plan")
           (make-jrecipe :name "plan" :args nil)))
  (should (equal
           (justl--get-recipe-from-file "./justfile" "push2")
           (make-jrecipe :name "push2" :args
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
           (make-jrecipe :name "default" :args nil)))
  (should (equal
           (justl--parse-recipe "build-cmd version='0.4':")
           (make-jrecipe
            :name "build-cmd"
            :args (list
                   (make-justl-jarg
                    :arg "version"
                    :default "'0.4'")))))
  (should (equal (justl--parse-recipe "push version version2:")
                 (make-jrecipe
                  :name "push"
                  :args (list (make-justl-jarg :arg "version" :default nil)
                              (make-justl-jarg :arg "version2" :default nil)))))
  (should (equal (justl--parse-recipe "push version: (build-cmd version)")
                 (make-jrecipe
                  :name "push"
                  :args (list (make-justl-jarg :arg "version" :default nil))))))

(ert "justl--*")

(provide 'justl-test)
;;; justl-test.el ends here
