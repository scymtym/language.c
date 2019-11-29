;;;; util.lisp --- Tests for the grammar rules of the cpp.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.shared.parser.test)

(defmacro define-rule-test (rule-and-options &body cases)
  (destructuring-bind (rule &key floating-constants?)
      (alexandria:ensure-list rule-and-options)
    (let ((test-name (alexandria:symbolicate '#:rule. rule))) ; TODO alexandria dependency
      `(test ,test-name
         ,(format nil "Smoke test for the `~(~A~)' rule." rule)
         (let (,@(when floating-constants?
                   `((language.c.shared.parser::*floating-point-constants?* t))))
           (architecture.builder-protocol:with-builder ('list)
             (parses-are (,rule) ,@cases)))))))
