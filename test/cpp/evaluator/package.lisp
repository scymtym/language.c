;;;; package.lisp --- Package definition for Unit tests of the cpp.evaluator module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.cpp.evaluator.test
  (:use
   #:cl

   #:fiveam

   #:language.c.cpp.evaluator)

  (:local-nicknames
   (#:parser #:language.c.cpp.parser)
   (#:model  #:language.c.cpp.model))

  (:export
   #:run-tests))

(cl:in-package #:language.c.cpp.evaluator.test)

(def-suite :language.c.cpp.evaluator)

(defun run-tests ()
  (run! :language.c.cpp.evaluator))
