;;;; builder.lisp --- A builder for making and relating model elements.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.cpp.model)

(defclass builder ()
  ())

(defmethod bp:make-node :around ((builder builder) (kind t)
                                 &key &allow-other-keys)
  (with-simple-restart (continue "Ignore the node")
    (call-next-method)))

(defmethod bp:make-node ((builder builder) (kind t)
                         &key &allow-other-keys)
  (error "Node kind ~A not implemented" kind))

(defmethod bp:relate :around ((builder  builder)
                              (relation t)
                              (left     t)
                              (right    null)
                              &key &allow-other-keys)
  left)

;;; Lexical elements

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :identifier))
                         &key name)
  (make-instance 'identifier :name name))

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :number))
                         &key value)
  (make-instance 'number* :value value))

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :string-literal))
                         &key value encoding)
  (declare (ignore encoding))
  (make-instance 'string-literal :value value))

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :punctuator))
                         &key which)
  (make-instance 'punctuator :which which))

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :header-name))
                         &key ((:kind kind*)) name)
  (make-instance 'header-name :kind kind* :name name))

;;; Line

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :line))
                         &key)
  (make-instance 'line))

(defmethod bp:relate ((builder builder)
                      (relation (eql :token))
                      (left     line)
                      (right    t)
                      &key)
  (vector-push-extend right (tokens left))
  left)

;;; Group

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :group))
                         &key)
  (make-instance 'group))

(defmethod bp:relate ((builder  builder)
                      (relation (eql :part))
                      (left     group)
                      (right    t)
                      &key)
  (vector-push-extend right (parts left))
  left)

;;; If

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :if))
                         &key
                         ((:kind kind*)))
  (make-instance 'if* :kind kind*))

(defmethod bp:relate ((builder  builder)
                      (relation (eql :test))
                      (left     if*)
                      (right    t)
                      &key)
  ;; (setf (%test left) right)
  (vector-push-extend right (test left))
  left)

(defmethod bp:relate ((builder  builder)
                      (relation (eql :then))
                      (left     if*)
                      (right    t)
                      &key)
  (setf (%then left) right)
  left)

(defmethod bp:relate ((builder  builder)
                      (relation (eql :else))
                      (left     if*)
                      (right    t)
                      &key)
  (setf (%else left) right)
  left)

;;; Control lines

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :include))
                         &key filename) ; TODO as relation
  (make-instance 'include :filename filename))

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :define-object-like-macro))
                         &key)
  (make-instance 'define-object-like-macro))

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :define-function-like-macro))
                         &key)
  (make-instance 'define-function-like-macro))

(defmethod bp:relate ((builder  builder)
                      (relation (eql :name))
                      (left     define)
                      (right    t)
                      &key)
  (setf (%name left) right)
  left)

(defmethod bp:relate ((builder  builder)
                      (relation (eql :replacement))
                      (left     define)
                      (right    t)
                      &key)
  (vector-push-extend right (replacement left))
  left)

(defmethod bp:relate ((builder  builder)
                      (relation (eql :parameter))
                      (left     define-function-like-macro)
                      (right    t)
                      &key)
  (vector-push-extend right (parameters left))
  left)

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :undef))
                         &key name)
  (make-instance 'undefine :name name))

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :error))
                         &key message)
  (make-instance 'error* :message message))

(defmethod bp:make-node ((builder builder)
                         (kind    (eql :pragma))
                         &key tokens)
  (make-instance 'pragma :tokens tokens))
