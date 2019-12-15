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
                      (builder     (make-instance 'model:builder))
                      (ast         (parser:parse input builder))
                      (environment (make-instance 'environment)))
                 (flet ((do-it ()
                          (with-output-to-string (stream)
                            (language.c.preprocessor.evaluator::output
                             (evaluate ast '() environment) stream))))
                   (case expected
                     (error (signals error (do-it)))
                     (t     (let ((expected (format nil expected)))
                              (is (string= expected (do-it))))))))))
       cases))

(test group.smoke
  "Smoke test for the evaluation of `group' nodes.")

(test if.smoke
  "Smoke test for the evaluation of `if' nodes."

  (eval-cases
   '("x"                                                 "x~%")

   '("#if 1~%x~%#endif"                                  "x~%")
   '("#if 0~%x~%#else~%y~%#endif"                        "y~%")
   '("#if 1~%x~%#else~%y~%#endif"                        "x~%")
   '("#if 0~%x~%#elif 0~%y~%#else~%z~%#endif"            "z~%")
   '("#if 0~%x~%#elif 1~%y~%#else~%z~%#endif"            "y~%")

   '("#if defined 1~%x~%#endif"                          error)
   '("#if defined foo~%x~%#else~%y~%#endif"              "y~%")
   '("#define foo~%#if defined foo~%x~%#else~%y~%#endif" "x~%")))

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
   '("\"foo\"" "\"foo\"~%")
   '("#define foo(x) x~%foo(1~%" error)
   '("#define foo(x,y) x + y~%baz foo(foo(1,2),foo(3,4)"
     error)

   '("#define foo 1, 2~@
      #define bar(x,y) x+y~@
      bar(foo)"
     "1+2~%")

   '("#define foo(x,y) x + y~%baz foo(foo(1,2),foo(3,4))"
     "baz 1+2+3+4~%")
   '("#define foo(x,y) x + y~%#define bar foo(3,4)~%baz foo(foo(1,2),bar)"
     "baz 1+2+3+4~%")

   '("#define foo(x,y) x + y~@
      baz foo(foo(1,2),foo(3,4))~%"
     "baz 1+2+3+4~%")
   '("#define foo(x,y) x + y~@
      #define foo2(x,y) x + y~@
      #define bar foo2(3,4)~@
      baz foo(foo(1,2),bar)"
     "baz 1+2+3+4~%")

   '("#define foo(x) x~@
     foo(x)"
     "x~%")

   '("#define foo foo~@
      foo"
     "foo~%")

   '("#define foo(x,...) x __VA_ARGS__~@
      foo(1,2,3,4)"
     "1 2 3 4~%")

   '("#define foo bar~@
      foo foo"
     "bar bar~%")

   '("'foo'"
     "'foo'~%")))
