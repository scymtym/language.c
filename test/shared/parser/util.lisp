;;;; util.lisp --- Tests for the grammar rules of the cpp.parser module
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.shared.parser.test)

(defmacro define-rule-test (rule-and-options &body cases)
  (destructuring-bind (rule &key skippable
                                 floating-constants?
                                 extended-unary-expressions)
      (alexandria:ensure-list rule-and-options)
    (let ((test-name (alexandria:symbolicate '#:rule. rule)))
      `(test ,test-name
         ,(format nil "Smoke test for the `~(~A~)' rule." rule)
         (let (,@(when skippable
                   `((language.c.shared.parser::*skippable-mode*
                      ,(ecase skippable
                         (:same-line :whitespace/same-line)
                         (:all       :whitespace)))))
               ,@(when floating-constants?
                   `((language.c.shared.parser::*floating-point-constants?* t)))
               ,@(when extended-unary-expressions
                   `((language.c.shared.parser::*extended-unary-expression*
                      ',extended-unary-expressions))))
           (architecture.builder-protocol:with-builder ('list)
             (parses-are (,rule) ,@cases)))))))
