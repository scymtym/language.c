;;;; package.lisp --- Package definition for tests of the interface module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.interface.test
  (:use
   #:cl

   #:fiveam

   #:language.c.interface)

  (:export
   #:run-tests))

(cl:in-package #:language.c.interface.test)

(def-suite :language.c.interface)

(defun run-tests ()
  (run! :language.c.interface))
