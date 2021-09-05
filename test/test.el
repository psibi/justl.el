(require 'justl)
(require 'ert)

(ert-deftest just--get-recipies-test ()
  (should (equal (list "default" "build-cmd" "push") (just--get-recipies))))

(ert-deftest just--list-to-recipe-test ()
  (should (equal (jrecipe-name (just--list-to-jrecipe (list "recipe" "arg"))) "recipe"))
  (should (equal (jrecipe-name (just--list-to-jrecipe (list "recipe"))) "recipe"))
  (should (equal (jrecipe-args (just--list-to-jrecipe (list "recipe"))) nil)))

(ert-deftest just--is-recipe-line-test ()
  (should (equal (just--is-recipe-line "default:") t)))



(ert "just--*")
