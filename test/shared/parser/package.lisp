;;;; package.lisp --- Package definition for tests of the shared.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.shared.parser.test
  (:use
   #:cl

   #:fiveam

   #:language.c.shared.parser)

  (:import-from #:parser.common-rules.test
   #:parses-are)

  ;; Test Utilities
  (:export
   #:define-rule-test)

  (:export
   #:run-tests))

(cl:in-package #:language.c.shared.parser.test)

(def-suite :language.c.shared.parser)

(defun run-tests ()
  (run! :language.c.shared.parser))
