;;;; package.lisp --- Package definition for test of the preprocessor.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.preprocessor.parser.test
  (:use
   #:cl

   #:fiveam

   #:language.c.preprocessor.parser)

  (:export
   #:run-tests))

(cl:in-package #:language.c.preprocessor.parser.test)

(defmacro define-rule-test (name &body body)
  `(language.c.shared.parser.test:define-rule-test
       (,name :skippable :same-line :floating-constants? nil)
     ,@body))

(def-suite :language.c.preprocessor.parser)

(defun run-tests ()
  (run! :language.c.preprocessor.parser))
