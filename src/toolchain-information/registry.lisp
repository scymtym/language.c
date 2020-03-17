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
  "Return information for the toolchain designated by ID.

   IF-DOES-NOT-EXIST controls the behavior in cases no information is
   available for the supplied ID. If the value is a function or the
   symbol `cl:error', it is called with an error condition as the sole
   argument. Otherwise the supplied value is returned from
   `find-information'.

   The returned information is a plist which at least contains the
   following properties:

   1) Key `:search-path', value a list of pathnames with form the
      compiler search path for includes. Elements closer to the head
      of the list aught to be tried before elements closer to the
      tail.

   2) Key `:builtin-macros', value a function that can be called with
      an environment as the sole argument. The function adds macro
      entries to the supplied environment and returns the modified
      environment.

   The returned value is mainly intended to be used with
   `augment-environment!'."
  (or (gethash id *toolchains*)
      (typecase if-does-not-exist
        ((or function (eql error))
         (funcall if-does-not-exist (make-no-toolchain-information id)))
        (t
         if-does-not-exist))))

(defun register-information (id search-path builtin-macros)
  "Install SEARCH-PATH and BUILTIN-MACROS as information for ID.

   The values of SEARCH-PATH and BUILTIN-MACROS should conform to the
   description given for `find-information'.

   Existing information for ID is replaced."
  (setf (gethash id *toolchains*) (list :search-path    search-path
                                        :builtin-macros builtin-macros)))
