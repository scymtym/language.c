;;;; protocol.lisp --- Protocol functions provided by the cpp.parser module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.cpp.parser)

(defgeneric parse (input builder &key rule))

(defmethod parse ((input string) (builder t) &key (rule 'preprocessing-file))
  (bp:with-builder (builder)
    (esrap:parse rule input)))

(defmethod parse ((input stream) (builder t) &rest args &key rule)
  (declare (ignore rule))
  (let ((input (read-stream-content-into-string input)))
    (apply #'parse input builder args)))

(defmethod parse ((input pathname) (builder t) &rest args &key rule)
  (declare (ignore rule))
  (let ((input (read-file-into-string input)))
    (apply #'parse input builder args)))
