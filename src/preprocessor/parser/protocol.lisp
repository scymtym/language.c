;;;; protocol.lisp --- Protocol functions provided by the preprocessor.parser module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.preprocessor.parser)

(defgeneric parse (input builder &key rule)
  (:documentation
   "Parse INPUT according to RULE, return result built with BUILDER.

    INPUT has to be convertible to a sequences characters. As such,
    strings, streams and pathnames are always supported while methods
    for other types of inputs may be defined.

    BUILDER controls the construction of the result (usually a parse
    tree) that the function will return in case of a successful
    parse.

    RULE, if supplied, specifies (generally restricts) the part of the
    grammar that should be used for parsing INPUT. This allows, for
    example, checking whether a given input conforms to the syntax of
    a specific element of the language."))

(declaim (inline %parse))
(defun %parse (rule input)
  ;; Configure the variable rules of the shared grammar for C language
  ;; syntax.
  (let ((language.c.shared.parser::*skippable-mode*            :whitespace/same-line)
        (language.c.shared.parser::*floating-point-constants?* nil)
        (language.c.shared.parser::*extended-unary-expression* 'unary-expression))
    (esrap:parse rule input)))

(defmethod parse ((input string) (builder t) &key (rule 'preprocessing-file))
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
