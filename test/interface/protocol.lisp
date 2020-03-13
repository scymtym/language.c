;;;; protocol.lisp --- Test for protocol functions of the interface module
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.interface.test)

(in-suite :language.c.interface)

(test preprocess.smoke
  "Smoke test for the `preprocess' function."

  (is (string= (format nil "1,2+3~%")
               (with-output-to-string (stream)
                 (language.c.interface:preprocess "#define foo 1, 2
#define bar(x,y) x+y
bar(foo,3)" :target stream)))))
