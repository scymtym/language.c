;;;; protocol.lisp --- Protocol provided by the cpp.evaluator module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.cpp.evaluator)

;;; Environment protocol

(defgeneric lookup (name environment))

(defgeneric (setf lookup) (new-value name environment))

(defgeneric expand (name environment))

(defgeneric substitution (name macro environment)) ; TODO better name?

;;; File protocol
;;;
;;; Allows tracking the currently processed file as well as resolving
;;; and reading included files.

(defgeneric push-file (file environment))

(defgeneric pop-file (environment))

(defgeneric include (file environment))

(defgeneric resolve-include (environment kind name)) ; TODO environment should be last parameter

;;; Evaluation protocol

;; TODO rename first parameter to node?
(defgeneric evaluate (element environment target)
  (:argument-precedence-order environment element target)
  (:documentation
   "Evaluate ELEMENT in ENVIRONMENT writing the result to TARGET."))

;;; Default behavior

(defmethod evaluate ((element sequence) (environment t) (target stream))
  (let ((first? t))
    (labels ((rec (remainder)
               (if first?
                   (setf first? nil)
                   (write-char #\Space target))
               (when (consp remainder)
                 (destructuring-bind (first . rest) remainder
                   (let ((result (evaluate first environment target)))
                     (typecase result
                       (string ; TODO currently needed for identifiers that evaluate to strings
                        (write-string result target)
                        (rec rest))
                                        ; (function expansion)
                       (function
                        (rec (funcall result rest environment target)))
                       (sequence
                        (rec (append (coerce result 'list) rest)))
                       (t
                        (rec rest))))))))
      (rec (coerce element 'list)))))

(defmethod resolve-include ((environment t) (kind t) (name t))
  nil)

(defmethod resolve-include :around ((environment t) (kind t) (name t))
  (or (call-next-method)
      (error "~@<Could not resolve ~A include ~S.~@:>" kind name)))

;;; Utilities

(defun evaluate-to-string (element environment)
  (with-output-to-string (stream)
    (evaluate element environment stream)))
