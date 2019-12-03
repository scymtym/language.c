;;;; evaluator.lisp --- Unit tests for the evaluator.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.preprocessor.evaluator.test)

(in-suite :language.c.preprocessor.evaluator)

#+no (let* ((group (language.c.preprocessor.parser:parse
               "#if !(defined FOO) && (BAR < 10)
#endif"
               (make-instance 'model:builder)))
       (string (evaluate-to-string
                (model:test (first-elt (model:parts group)))
                (make-instance 'undefined-is-0-environment :parent (make-instance 'environment))))
       (test-ast
         (architecture.builder-protocol:with-builder ('list)
           (esrap:parse 'language.c.shared.parser::constant-expression string))))
  (list string test-ast (eval-constant-expression test-ast)))

(defun eval-cases (&rest cases)
  (map nil (lambda (case)
             (destructuring-bind (input expected) case
               (let* ((input       (format nil input))
                      (expected    (format nil expected))
                      (builder     (make-instance 'model:builder))
                      (ast         (parser:parse input builder))
                      (environment (make-instance 'environment))
                      (result      (with-output-to-string (stream)
                                     (evaluate ast environment stream))))
                 (is (string= expected result)))))
       cases))

(test group.smoke
  "Smoke test for the evaluation of `group' nodes.")

(test if.smoke
  "Smoke test for the evaluation of `if' nodes."

  (eval-cases
   '("x"                          "x~%")
   '("#if 1~%x~%#endif"           "x~%")
   '("#if 0~%x~%#else~%y~%#endif" "y~%")
   '("#if 1~%x~%#else~%y~%#endif" "x~%")))

(test include.smoke
  "Smoke test for the evaluation of `include' nodes.")

(test define-object-like-macro.smoke
  "Smoke test for the evaluation of `define-object-like-macro' nodes."

  (eval-cases
   '("#define foo 1~%foo" "1~%")))

(test define-function-like-macro.smoke
  "Smoke test for the evaluation of `define-function-like-macro' nodes."

  (eval-cases
   '("#define foo(x) x~%foo(1)" "1~%"))
  )

;;;

(test unsorted.smoke                    ; TODO obviously
  "Unsorted tests"
  (eval-cases
   '("\"foo\"" "\"foo\"~%")))
