;;;; registry.lisp --- Toolchain information registry
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.toolchain-information)

(defvar *toolchains* (make-hash-table :test #'equal))

(defun make-no-toolchain-information (id)
  (make-instance 'simple-error
                 :format-control   "No information for toolchain ~S"
                 :format-arguments (list id)))

(defun find-information (id &key (if-does-not-exist #'error))
  (or (gethash id *toolchains*)
      (typecase if-does-not-exist
        ((or function (eql error))
         (funcall if-does-not-exist (make-no-toolchain-information id)))
        (t
         if-does-not-exist))))

(defun register-information (id search-path builtin-macros)
  (setf (gethash id *toolchains*) (list :search-path    search-path
                                        :builtin-macros builtin-macros)))
