;;;; protocol.lisp --- Protocol provided by the preprocessor.evaluator module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.preprocessor.evaluator)

;;; Environment protocol

(defgeneric lookup (name environment))

(defgeneric (setf lookup) (new-value name environment))

;;; File protocol
;;;
;;; Allows tracking the currently processed file as well as resolving
;;; and reading included files.

(defgeneric push-file (file environment))

(defgeneric pop-file (environment))

(defgeneric include (file environment))

(defgeneric resolve-include (environment kind name)) ; TODO environment should be last parameter

;;; Default behavior

(defmethod resolve-include ((environment t) (kind t) (name t))
  nil)

(defmethod resolve-include :around ((environment t) (kind t) (name t))
  (or (call-next-method)
      (error "~@<Could not resolve ~A include ~S.~@:>" kind name)))

;;; Evaluation protocol

;; TODO rename first parameter to node?
(defgeneric evaluate (element remainder environment)
  (:argument-precedence-order environment element remainder)
  (:documentation
   "Evaluate ELEMENT with successors REMAINDER in ENVIRONMENT"))

(defgeneric evaluate-pragma (which element environment)
  (:documentation
   "Evaluate the pragma indicated by WHICH in ENVIRONMENT."))

(defgeneric output (element target)
  (:documentation
   "TODO"))

(defun evaluate-to-string (element environment)
  (with-output-to-string (stream)
    (output (evaluate element '() environment) stream)))

;;; Default behavior

(defun identifier= (left right) ; TODO move to model? implement token= generic function?
  (and (typep left 'model:identifier)
       (typep right 'model:identifier)
       (string= (model:name left) (model:name right))))

(defmethod evaluate ((element sequence) (remainder t) (environment t))
  (unless (and (emptyp element) (emptyp remainder))
    (values (loop :with (first . rest) = (append (coerce element 'list) (coerce remainder 'list)) ; TODO slow
                  :with suppressed     = '()
                  :for r = rest :then (rest new-remainder)
                  :for e = first :then (first new-remainder)
                  :for (value new-remainder new-suppressed)
                     = (if (find e suppressed :test #'identifier= :key #'car)
                           (list (list e) r)
                           (multiple-value-list
                            (evaluate e r environment)))
                  :appending value
                  :do (let ((reduced (delete e suppressed :test #'eq :key #'cdr)))
                        (setf suppressed (if new-suppressed
                                             (list* new-suppressed reduced)
                                             reduced)))
                  :while new-remainder)
            '())))

(defmethod output ((element character) (target stream))
  (write-char element target))

(defmethod output ((element list) (target stream))
  (loop :for previous = nil :then token
        :for token :in element
        :when (and previous
                   (typep previous '(or model::identifier model::number*))
                   (typep token '(or model::identifier model::number*)))
        :do (write-char #\Space target)
        :do (output token target)))
