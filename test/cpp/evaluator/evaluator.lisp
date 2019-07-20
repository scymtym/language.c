;;;; evaluator.lisp --- Unit tests for the evaluator.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.cpp.evaluator.test)

(in-suite :language.c.cpp.evaluator)

#+no (let* ((group (language.c.cpp.parser:parse
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
  (map nil (lambda+ ((input expected))
             (let* ((input       (format nil input))
                    (expected    (format nil expected))
                    (builder     (make-instance 'model:builder))
                    (ast         (parser:parse input builder))
                    (environment (make-instance 'environment))
                    (result      (with-output-to-string (stream)
                                   (evaluate ast environment stream))))
               (is (string= expected result))))
       cases))

(test if.smoke

  (eval-cases
   '("x"                          "x~%")
   '("#if 1~%x~%#endif"           "x~%")
   '("#if 0~%x~%#else~%y~%#endif" "y~%")
   '("#if 1~%x~%#else~%y~%#endif" "x~%")))
