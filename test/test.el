(require 'justl)
(require 'ert)

(ert-deftest just--get-recipies-test ()
  (should (equal (list "default" "build-cmd" "push") (just--get-recipies))))

(ert 'just--get-recipies-test)
