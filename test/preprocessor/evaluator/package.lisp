;;;; package.lisp --- Package definition for Unit tests of the preprocessor.evaluator module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.preprocessor.evaluator.test
  (:use
   #:cl

   #:fiveam

   #:language.c.preprocessor.evaluator)

  (:local-nicknames
   (#:parser #:language.c.preprocessor.parser)
   (#:model  #:language.c.preprocessor.model))

  (:export
   #:run-tests))

(cl:in-package #:language.c.preprocessor.evaluator.test)

(def-suite :language.c.preprocessor.evaluator)

(defun run-tests ()
  (run! :language.c.preprocessor.evaluator))
