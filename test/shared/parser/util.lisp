;;;; util.lisp --- Tests for the grammar rules of the cpp.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.shared.parser.test)

(defmacro define-rule-test (rule &body cases)
  (let ((test-name (alexandria:symbolicate '#:rule. rule))) ; TODO alexandria dependency
    `(test ,test-name
       ,(format nil "Smoke test for the `~(~A~)' rule." rule)
       (architecture.builder-protocol:with-builder ('list)
         (parses-are (,rule) ,@cases)))))
