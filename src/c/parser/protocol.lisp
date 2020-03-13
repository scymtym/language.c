;;;; protocol.lisp --- Protocol function provided by the c.parser module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.c.parser)

(defgeneric parse (input builder &key rule))

(declaim (inline %parse))
(defun %parse (rule input)
  (let ((language.c.shared.parser::*skippable-mode*            :whitespace)
        (language.c.shared.parser::*floating-point-constants?* t)
        (language.c.shared.parser::*extended-unary-expression* 'cast-expression))
   (esrap:parse rule input)))

(defmethod parse ((input string) (builder t) &key (rule 'translation-unit/whitespace))
  (bp:with-builder (builder)
    (%parse rule input)))

(defmethod parse ((input stream) (builder t) &rest args &key rule)
  (declare (ignore rule))
  (let ((input (read-stream-content-into-string input)))
    (apply #'parse input builder args)))

(defmethod parse ((input pathname) (builder t) &rest args &key rule)
  (declare (ignore rule))
  (let ((input (read-file-into-string input)))
    (apply #'parse input builder args)))
