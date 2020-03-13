;;;; package.lisp --- Package definition for tests of the c.parser module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.c.parser.test
  (:use
   #:cl

   #:fiveam

   #:language.c.c.parser)

  (:shadowing-import-from #:language.c.c.parser
   #:keyword
   #:declaration)

  (:export
   #:run-tests))

(cl:in-package #:language.c.c.parser.test)

(defmacro define-rule-test (name &body body)
  `(language.c.shared.parser.test:define-rule-test
       (,name :skippable                  :all
              :floating-constants?        t
              :extended-unary-expressions language.c.c.parser::cast-expression)
     ,@body))

(def-suite :language.c.c.parser)

(defun run-tests ()
  (run! :language.c.c.parser))
