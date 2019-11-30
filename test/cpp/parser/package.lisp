;;;; grammar.lisp --- Tests for the grammar rules of the cpp.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.cpp.parser.test
  (:use
   #:cl

   #:fiveam

   #:language.c.cpp.parser)

  (:export
   #:run-tests))

(cl:in-package #:language.c.cpp.parser.test)

(defmacro define-rule-test (name &body body)
  `(language.c.shared.parser.test:define-rule-test
       (,name :skippable :same-line :floating-constants? nil)
     ,@body))

(def-suite :language.c.cpp.parser)

(defun run-tests ()
  (run! :language.c.cpp.parser))
