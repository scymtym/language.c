;;;; package.lisp --- Package definition for tests of the c.parser module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.c.parser.test
  (:use
   #:cl

   #:fiveam

   #:language.c.c.parser)

  (:import-from #:language.c.shared.parser.test
   #:define-rule-test)

  (:export
   #:run-tests))

(cl:in-package #:language.c.c.parser.test)

(def-suite :language.c.c.parser)

(defun run-tests ()
  (run! :language.c.c.parser))
