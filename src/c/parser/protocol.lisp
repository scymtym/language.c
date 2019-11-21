;;;; protocol.lisp --- Protocol function provided by the c.parser module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.c.parser)

(defgeneric parse (input builder &key rule))

(defmethod parse ((input string) (builder t) &key (rule 'translation-unit/whitespace))
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
