;;;; augment.lisp --- Augment environments with toolchain information.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.toolchain-information)

(defgeneric augment-environment! (environment information &key search-path)
  (:documentation
   "Augment ENVIRONMENT with toolchain information INFORMATION.

    In more detail, builtin macro definitions and include search path
    entries from INFORMATION are added to ENVIRONMENT.

    If SEARCH-PATH is supplied, the supplied value is used instead of
    the one contained in INFORMATION. In either case, the previous
    search path of ENVIRONMENT is replaced, not augmented.

    Return destructively modified ENVIRONMENT"))

(defmethod augment-environment!
    ((environment t) (information cons)
     &key (search-path (getf information :search-path)))
  (let ((builtin-macros (getf information :builtin-macros)))
    (reinitialize-instance environment :search-path search-path)
    (funcall builtin-macros (lambda (name value)
                              (setf (eval:lookup name environment) value))))
  environment)

(defmethod augment-environment! ((environment t) (information string) 
                                 &rest args &key search-path)
  (declare (ignore search-path))
  (let ((information (find-information information)))
    (apply #'augment-environment! environment information args)))

(defmethod augment-environment! ((environment t) (information (eql t))
                                 &rest args &key search-path)
  (declare (ignore search-path))
  (apply #'augment-environment! environment (guess-toolchain-id) args))
