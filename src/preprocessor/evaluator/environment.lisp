;;;; environment.lisp --- Environment for evaluating C expressions.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.preprocessor.evaluator)

;;; Macros
;;;
;;; Instances of these classes represent macros defined via
;;;
;;;   #define NAME                          // -> `empty-macro'
;;;   #define NAME REPLACEMENT              // -> `object-like-macro'
;;;   #define NAME(PARAMETERS) REPLACEMENT  // -> `function-like-macro'
;;;
;;; and are usually stored in environments under the supplied NAME.

(defclass macro ()
  ())

(defclass empty-macro (macro)
  ())

(defclass replacement-macro (macro)
  ((%replacement :initarg :replacement
                 :reader  replacement)))

(defclass object-like-macro (replacement-macro)
  ())

(defclass function-like-macro (replacement-macro)
  ((%parameters :initarg :parameters
                :reader  parameters)))

;;; `environment'

(defclass environment ()
  ((%entries :reader   entries
             :initform (make-hash-table :test #'equal))))

(defmethod lookup ((name t) (environment environment))
  (gethash name (entries environment)))

(defmethod (setf lookup) ((new-value t) (name t) (environment environment))
  (setf (gethash name (entries environment)) new-value))

(defmethod expand ((name t) (environment t))
  (let ((macro (lookup name environment)))
    (substitution name macro environment)))

(defmethod substitution ((name t) (macro null) (environment environment))
  name)

;;; `search-path-environment'

(defclass search-path-environment (environment)
  ((%search-path :initarg :search-path
                 :reader  search-path)))

(defmethod resolve-include ((environment environment)
                            (kind        (eql :system))
                            (name        string))

  (or (some (lambda (entry)
              (probe-file (merge-pathnames name entry)))
            (search-path environment))
      (call-next-method)))

;;; `file-environment'

(defclass file-environment (environment)
  ((%file :initarg :file
          :reader  file)))

(defmethod resolve-include ((environment file-environment)
                            (kind        t)
                            (name        string))
  (or (probe-file (merge-pathnames name (file environment)))
      (call-next-method)))

;;; `include-environment'

(defclass include-environment (file-environment
                               search-path-environment)
  ((%include-stack  :initarg  :include-stack
                    :reader   include-stack
                    :accessor %include-stack
                    :initform ())
   (%included-files :initarg  :included-files
                    :reader   included-files
                    :initform (make-hash-table :test #'equalp))))

(defmethod push-file ((file pathname) (environment include-environment))
  (push file (%include-stack environment)))

(defmethod pop-file ((environment include-environment))
  (pop (%include-stack environment)))

(defmethod include ((file        pathname)
                    (environment include-environment))
  (assert (uiop:absolute-pathname-p file))
  (let ((included-files (included-files environment)))
    (or (gethash file included-files)
        (setf (gethash file included-files)
              (alexandria:read-file-into-string file)))))

;;;

(defclass child-environment-mixin ()
  ((%parent :initarg :parent
            :reader  parent)))

(defmethod lookup ((name t) (environment child-environment-mixin))
  (lookup name (parent environment)))

;;;

(defclass child-environment (child-environment-mixin
                             environment)
  ())

(defmethod lookup ((name t) (environment child-environment))
  (or (gethash name (entries environment)) ; TODO avoid direct access
      (call-next-method)))

;;;

(defclass argument-collecting-environment (child-environment-mixin)
  ((%call-environment :initarg  :call-environment
                      :reader   %call-environment)
   (%state            :accessor state
                      :initform :open-paren)
   (%argument         :accessor %argument
                      :initform '())
   (%parameters       :initarg  :parameters
                      :accessor parameters)))

(defmethod call-environment ((object argument-collecting-environment))
  (unless (eq (state object) :done)
    (error "Incomplete macro call expression"))
  (%call-environment object))

(defmethod evaluate ((element     model::punctuator)
                     (remainder   t)
                     (environment argument-collecting-environment))
  (flet ((state-transition (got new)
           (let ((expected (state environment)))
             (unless (eq expected got)
               (error "Expected ~(~A~) but got ~(~A~) for macro ~A"
                      expected got :TODO)))
           (setf (state environment) new))
         (flush-argument ()
           (let ((name (pop (parameters environment))))
             (setf (lookup name (%call-environment environment))
                   (make-instance 'object-like-macro :replacement (%argument environment)))
             (setf (%argument environment) '())
             (setf (state environment) (cond ((parameters environment)
                                              :comma)
                                             ;; TODO ellipsis
                                             (t
                                              :close-paren))))))
    (case (model::which element)
      (:|(|
       (state-transition :open-paren :argument)
       (values '() remainder))
      (:|,|
       (flush-argument)
       (state-transition :comma :argument)
       (values '() remainder))
      (:|)|
       (when (parameters environment)
         (flush-argument))
       (state-transition :close-paren :done)
       (values '() remainder t))
      (t
       (call-next-method)))))

(defmethod evaluate ((element     standard-object)
                     (remainder   t)
                     (environment argument-collecting-environment))
  (let ((state (state environment)))
    (unless (eq state :argument)
      (error "Expected ~(~A~) but got argument (~A) macro ~A"
             state element :TODO)))
  (multiple-value-bind (value remainder)
      (evaluate element remainder (parent environment))
    (appendf (%argument environment) value)
    (values '() remainder)))

;;; `test-environment'
;;;
;;; An environment with special evaluation rules for evaluating the
;;; test of an `if' node.

(defclass test-environment (child-environment-mixin)
  ())

(defmethod evaluate ((element     model:identifier)
                     (environment test-environment)
                     (target      t))
  (if (string= (model:name element) "defined")
      (lambda (remainder environment target)
        (destructuring-bind (first &rest rest) remainder
          (let ((defined? (lookup (model:name first) environment)))
            (write-string (if defined? "1" "0") target))
          rest))
      (call-next-method)))

(defmethod substitution ((name        t)
                         (macro       null)
                         (environment test-environment))
  "0")

(defmethod substitution ((name        t)
                         (macro       empty-macro)
                         (environment test-environment))
  "1")

(defmethod substitution ((name        t)
                         (macro       object-like-macro)
                         (environment test-environment))
  (replacement macro))
