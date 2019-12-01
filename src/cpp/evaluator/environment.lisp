;;;; environment.lisp --- Environment for evaluating C expressions.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.cpp.evaluator)

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

(defmethod substitution ((name        t)
                         (macro       empty-macro)
                         (environment environment))
  "")

(defmethod substitution ((name        t)
                         (macro       object-like-macro)
                         (environment environment))
  (replacement macro))

(defmethod substitution ((name        t)
                         (macro       function-like-macro)
                         (environment t))
  (let ((parameters  (map 'list 'model:name (parameters macro)))
        (replacement (replacement macro)))
    (lambda (remainder environment target)
      (let* ((call-environment     (make-instance 'child-environment
                                                  :parent environment))
             (argument-environment (make-instance 'argument-collecting-environment
                                                  :call-environment call-environment
                                                  :parameters       parameters
                                                  :parent           environment)))
        (loop :for (first . rest) :on remainder
              :while (evaluate first argument-environment target)
              :finally (progn
                         (evaluate replacement call-environment target)
                         (return rest)))))))

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
                      :reader   call-environment)
   (%parameters       :initarg  :parameters
                      :accessor parameters)))

(defmethod evaluate ((element     model::punctuator)
                     (environment argument-collecting-environment)
                     (target      t))
  (case (model::which element)
    ((:|(| :|,|)
     t)
    (:|)|
     nil)
    (t
     (call-next-method))))

(defmethod evaluate ((element     standard-object)
                     (environment argument-collecting-environment)
                     (target      t))
  (let ((name  (pop (parameters environment)))
        (value (block nil
                 (with-output-to-string (stream)
                   (let ((result (evaluate element (parent environment) stream)))
                     (when (stringp result)
                       (return result)))))))
    (setf (lookup name (call-environment environment))
          (make-instance 'object-like-macro :replacement value))))

;;;

;; TODO test-environment
(defclass undefined-is-0-environment (child-environment-mixin)
  ())

(defmethod evaluate ((element     model:identifier)
                     (environment undefined-is-0-environment)
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
                         (environment undefined-is-0-environment))
  "0")

(defmethod substitution ((name        t)
                         (macro       empty-macro)
                         (environment undefined-is-0-environment))
  "1")

(defmethod substitution ((name        t)
                         (macro       object-like-macro)
                         (environment undefined-is-0-environment))
  (replacement macro))
