(require 'justl)
(require 'ert)

(ert-deftest just--get-recipies-test ()
  (should (equal (list "default" "build-cmd" "push") (just--get-recipies))))

(ert-deftest just--list-to-recipe-test ()
  (should (equal (jrecipe-name (just--list-to-jrecipe (list "recipe" "arg"))) "recipe"))
  (should (equal (jrecipe-name (just--list-to-jrecipe (list "recipe"))) "recipe"))
  (should (equal (jrecipe-args (just--list-to-jrecipe (list "recipe"))) nil)))

(ert-deftest just--is-recipe-line-test ()
  (should (equal (just--is-recipe-line "default:") t))
  (should (equal (just--is-recipe-line "build-cmd version='0.4':") t))
  (should (equal (just--is-recipe-line "push version: (build-cmd version)") t))
  (should (equal (just--is-recipe-line "    just --list") nil)))

(ert-deftest just--find-justfiles-test ()
  (should (equal (length (just--find-justfiles ".")) 1)))

(ert "just--*")
