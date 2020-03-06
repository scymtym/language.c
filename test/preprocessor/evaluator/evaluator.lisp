;;;; evaluator.lisp --- Unit tests for the evaluator.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.preprocessor.evaluator.test)

(in-suite :language.c.preprocessor.evaluator)

(defun eval-cases (&rest cases)
  (map nil (lambda (case)
             (destructuring-bind (input expected) case
               (let* ((input       (format nil input))
                      (builder     (make-instance 'model:builder))
                      (ast         (parser:parse input builder))
                      (environment (make-instance 'environment)))
                 (flet ((do-it ()
                          (evaluate-to-string ast environment)))
                   (case expected
                     (error (signals error (do-it)))
                     (t     (let* ((expected (format nil expected))
                                   (result   (do-it)))
                              (is (string= expected result)
                                  "For input ~S, expected ~S but got ~S."
                                  input expected result))))))))
       cases))

(test lexical-elements.smoke
  "Smoke test for the evaluation of lexical nodes."

  (eval-cases
   '("foo"     "foo~%")
   '("'1'"     "'1'~%")
   '("1"       "1~%")
   '("0x10"    "0x10~%")
   '("\"foo\"" "\"foo\"~%")))

(test group.smoke
  "Smoke test for the evaluation of `group' nodes."

  (eval-cases
   '("1"    "1~%")
   '("1~%2" "1~%2~%")))

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
   '("#if defined(foo)~%x~%#else~%y~%#endif"             "y~%")
   '("#define foo~%#if defined foo~%x~%#else~%y~%#endif" "x~%")

   '("#ifdef foo~%x~%#else~%y~%#endif"                   "y~%")
   '("#define foo~%#ifdef foo~%x~%#else~%y~%#endif"      "x~%")

   '("#ifndef foo~%x~%#else~%y~%#endif"                  "x~%")
   '("#define foo~%#ifndef foo~%x~%#else~%y~%#endif"     "y~%")

   ;; More complex cases
   '("#if bar == 0~%x~%#else~%y~%#endif"                 "x~%")
   '("#if bar == 1~%x~%#else~%y~%#endif"                 "y~%")
   '("#if bar < 0~%x~%#else~%y~%#endif"                  "y~%")
   '("#if bar < 1~%x~%#else~%y~%#endif"                  "x~%")
   '("#if !(defined FOO) && (BAR < 10)~%x~%#endif"       "x~%")))

(test include.smoke
  "Smoke test for the evaluation of `include' nodes."

  (eval-cases
   '("#include <does-not-exist>"     error)
   '("#include \"does-not-exist.h\"" error)))

(test define-object-like-macro.smoke
  "Smoke test for the evaluation of `define-object-like-macro' nodes."

  (eval-cases
   '("#define foo 1"            "")
   '("#define foo 1~%foo"       "1~%")

   '("#define foo foo~%foo"     "foo~%")
   '("#define foo bar~%foo foo" "bar bar~%")))

(test define-function-like-macro.smoke
  "Smoke test for the evaluation of `define-function-like-macro' nodes."

  (eval-cases
   '("#define foo() x"                                "")
   '("#define foo(x) x"                               "")

   '("#define foo() x~%foo()"                         "x~%")
   '("#define foo(x) x~%foo(1)"                       "1~%")

   '("#define foo(x,...) x __VA_ARGS__~%foo(1,2,3,4)" "1 2 3 4~%")

   ;; Nested parentheses in call
   '("#define foo(x) x~%foo(bar())"                   "bar()~%")

   ;; Multi-line macro invocation
   '("#define foo(x) x~%foo(bar(1,2)~%fez)"           "bar(1,2)~%fez~%")))

;;;

(test unsorted.smoke                    ; TODO obviously
  "Unsorted tests"

  (eval-cases
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
     "baz 1+2+3+4~%")))
