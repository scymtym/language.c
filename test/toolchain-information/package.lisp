;;;; package.lisp --- Package definition for tests of the toolchain-information module
;;;;
;;;; Copyright (C) 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.toolchain-information.test
  (:use
   #:cl

   #:fiveam

   #:language.c.toolchain-information)

  (:export
   #:run-tests))

(cl:in-package #:language.c.toolchain-information.test)

(def-suite :language.c.toolchain-information)

(defun run-tests ()
  (run! :language.c.toolchain-information))
