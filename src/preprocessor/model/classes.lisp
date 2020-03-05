;;;; classes.lisp --- Classes provided by the preprocessor.model module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.preprocessor.model)

;;; Utilities

(defun token->string (token)
  (typecase token
    (single-token-mixin
     (token-string token))
    (t
     (with-standard-io-syntax
       (princ-to-string token)))))

;;; Mixins

(defclass single-token-mixin ()
  ())

(defmethod print-items:print-items append ((object single-token-mixin))
  `((:token-string ,(token-string object) "~A")))

;;; Lexical elements

(defclass identifier (single-token-mixin
                      print-items:print-items-mixin)
  ((%name :initarg :name
          :type    string
          :reader  name
          :reader  token-string)))

;; TODO rename to number-literal
(defclass number* (single-token-mixin
                   print-items:print-items-mixin)
  ((%value :initarg :value
           :reader  value)))

(defmethod token-string ((node number*))
  (token->string (value node)))

(defclass string-literal (single-token-mixin
                          print-items:print-items-mixin)
  ((%value :initarg :value
           :reader  value
           :reader  token-string)))

(defclass punctuator (single-token-mixin
                      print-items:print-items-mixin)
  ((%which :initarg :which
           :reader  which)))

(defmethod token-string ((node punctuator)) ; TODO return the object and let evaluator stringify it?
  (string (which node)))

(defclass header-name (single-token-mixin
                       print-items:print-items-mixin)
  ((%kind :initarg :kind
          :type    (member :system :local)
          :reader  kind)
   (%name :initarg :name
          :type    string
          :reader  name
          :reader  token-string)))

(defmethod print-items:print-items append ((object header-name))
  `((:kind ,(kind object) "~A " ((:before :token-string)))))

;;;

(defclass group ()
  ((%parts :reader   parts
           :initform (make-array 1 :adjustable t :fill-pointer 0))))

;;;

(defclass line ()
  ((%tokens :reader   tokens
            :initform (make-array 1 :adjustable t :fill-pointer 0))))

;;;

(defclass if* ()
  ((%kind :initarg  :kind
          :type     (member :if :ifdef :ifndef)
          :reader   kind)
   ;; Test is currently a collection of tokens. Apparently, we must
   ;; macro-expand those, parse the result as a "constant-expression"
   ;; and then evaluate it.
   (%test :initarg  :test
          :reader   test
                                        ; :writer  (setf %test)
          :initform (make-array 1 :adjustable t :fill-pointer 0))
   (%then :initarg  :then
          :reader   then
          :writer   (setf %then)
          :initform nil)
   (%else :initarg  :else
          :reader   else
          :writer   (setf %else)
          :initform nil)))

;;; Control lines

(defclass include ()
  ((%filename :initarg  :filename
              :reader   filename
              :initform (make-array 1 :adjustable t :fill-pointer 0))))

(defclass define ()
  ((%name        :initarg  :name ; TODO is the name expanded first? otherwise make a name-mixin
                 :reader   name
                 :writer   (setf %name))
   (%replacement :initarg  :replacement
                 :reader   replacement
                 :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass define-object-like-macro (define)
  ())

(defclass define-function-like-macro (define)
  ((%parameters :reader   parameters
                :initform (make-array 0 :adjustable t :fill-pointer 0))
   (%ellipsis?  :initarg  :ellipsis?
                :reader   ellipsis?)))

(defclass undefine ()
  ((%name :initarg :name
          :reader  name
          :writer  (setf %name))))

;; TODO this is the #line control line, but classes with line class defined above
#+later (defclass line ()
          ())

(defclass error* ()
  ((%message :initarg  :message
             :reader   message
             :initform (make-array 1 :adjustable t :fill-pointer 0))))

(defclass pragma (utilities.print-items:print-items-mixin)
  ((%tokens :initarg  :tokens
            :reader   tokens
            :initform (make-array 1 :adjustable t :fill-pointer 0))))

(defmethod print-items:print-items append ((object pragma))
  `((:first-token ,(token->string (first (tokens object))))))
