;;;; environment.lisp --- Environment for evaluating C expressions.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
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

;;; `string-environment'

(defclass string-environment (environment)
  ())

;;; `search-path-environment'

(defclass search-path-environment (environment)
  ((%search-path :initarg :search-path
                 :reader  search-path)))

(defmethod resolve-include ((kind        (eql :system))
                            (name        string)
                            (environment environment))

  (or (some (lambda (entry)
              (probe-file (merge-pathnames name entry)))
            (search-path environment))
      (call-next-method)))

;;; `file-environment'

(defclass file-environment (environment)
  ((%file :initarg :file
          :reader  file)))

(defmethod resolve-include ((kind        t)
                            (name        string)
                            (environment file-environment))
  (let ((candidate (merge-pathnames name (file environment))))
    (or (probe-file candidate)
        (call-next-method))))

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

(defmethod file ((environment include-environment))
  (first (%include-stack environment)))

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

;;; `child-environment-mixin'

(defclass child-environment-mixin ()
  ((%parent :initarg :parent
            :reader  parent)))

(defmethod lookup ((name t) (environment child-environment-mixin))
  (lookup name (parent environment)))

;;; `child-environment'

(defclass child-environment (child-environment-mixin ; TODO is this used?
                             environment)
  ())

(defmethod lookup ((name t) (environment child-environment))
  (or (gethash name (entries environment)) ; TODO avoid direct access
      (call-next-method)))

;;; `argument-collecting-environment'

(defclass argument-collecting-environment (child-environment-mixin)
  ((%call-environment :initarg  :call-environment
                      :reader   %call-environment)
   (%state            :accessor state
                      :initform :open-paren)
   (%paren-depth      :accessor paren-depth
                      :initform 0)
   (%argument         :accessor %argument
                      :initform '())
   (%parameters       :initarg  :parameters
                      :accessor parameters)))

(defun make-argument-collecting-environment (parameters environment)
  (make-instance 'argument-collecting-environment
                 :call-environment (make-instance 'environment)
                 :parameters       parameters
                 :parent           environment))

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
           (let ((name (or (pop (parameters environment)) "__VA_ARGS__")))
             (setf (lookup name (%call-environment environment))
                   (make-instance 'object-like-macro
                                  :replacement (%argument environment)))
             (setf (%argument environment) '())
             (setf (state environment) (cond ((parameters environment)
                                              :comma)
                                             (t ; (model::ellipsis? )
                                              :comma/ellipsis)
                                             (t
                                              :close-paren))))))
    (case (model::which element)
      (:|(|
       (cond ((eq (state environment) :argument)
              (incf (paren-depth environment))
              (call-next-method))
             (t
              (state-transition :open-paren :argument)
              (values '() remainder))))
      (:|,|
       (cond ((plusp (paren-depth environment))
              (call-next-method))
             (t
              (when (eq (state environment) :argument)
                (flush-argument))
              (case (state environment)
                (:comma          (state-transition :comma          :argument))
                (:comma/ellipsis (state-transition :comma/ellipsis :rest-argument)))
              (values '() remainder))))
      (:|)|
       (cond ((plusp (paren-depth environment))
              (decf (paren-depth environment))
              (call-next-method))
             (t
              (flush-argument)
              (state-transition (if (eq (state environment) :comma/ellipsis)
                                    :comma/ellipsis
                                    :close-paren)
                                :done)
              (values '() remainder t))))
      (t
       (call-next-method)))))

(defmethod evaluate ((element     standard-object)
                     (remainder   t)
                     (environment argument-collecting-environment))
  (let ((state (state environment)))
    (unless (member state '(:argument :rest-argument))
      (error "Expected ~(~A~) but got argument (~A) macro ~A"
             state element :TODO)))
  (appendf (%argument environment) (list element))
  (values '() remainder)
  #+no (multiple-value-bind (value remainder)
           (evaluate element remainder (parent environment))
         (appendf (%argument environment) value)
         (values '() remainder)))

;;; `test-environment'
;;;
;;; An environment with special evaluation rules for evaluating the
;;; test of an `if' node.

(#+sbcl sb-ext:defglobal #-sbcl defvar **test-true**
  (list (make-instance 'model:number* :value "1")))

(#+sbcl sb-ext:defglobal #-sbcl defvar **test-false**
  (list (make-instance 'model:number* :value "0")))

(defclass test-environment (child-environment-mixin)
  ())

(defmethod evaluate ((element     model:identifier)
                     (remainder   t)
                     (environment test-environment))
  (let ((name (model:name element)))
    (cond ((string= name "defined")
           (destructuring-bind (first &optional second third &rest rest)
               remainder
             (cond ((and (typep first 'model::punctuator)
                         (eq (model::which first) :|(|)
                         (eq (model::which third) :|)|))
                    (values (if (lookup (model:name second) environment)
                                **test-true**
                                **test-false**)
                            rest))
                   ((not (typep first 'model:identifier))
                    (error "Argument of defined operator must be an identifier"))
                   (t
                    (values (if (lookup (model:name first) environment)
                                **test-true**
                                **test-false**)
                            (rest remainder))))))
          ((not (lookup name environment))
           (values **test-false** remainder))
          (t
           (call-next-method)))))

(defmethod evaluate ((element     empty-macro)
                     (remainder   t)
                     (environment test-environment))
  (values **test-true** remainder))

(defmethod evaluate ((element     t)
                     (remainder   object-like-macro)
                     (environment test-environment))
  (replacement remainder))
