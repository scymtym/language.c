;;;; protocol.lisp --- Protocol functions provided by the interface module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.interface)

(defvar *default-search-path*
  '(#+unix               #P"/usr/include/"
    #+linux              #P"/usr/include/linux/"
    #+(and linux x86-64) #P"/usr/include/x86_64-linux-gnu/"))

(defmethod preprocess ((source pathname)
                       &key (target      *standard-output*)
                            (search-path *default-search-path*))
  (let* ((builder     (make-instance 'language.c.cpp.model:builder))
         (ast         (language.c.cpp.parser:parse source builder))
         (environment (make-instance 'language.c.cpp.evaluator:include-environment
                                     :file        source ; TODO this should initialize include-stack and included-files
                                     :search-path search-path
                                     :include-stack (list source)
                                     :included-files (let ((h (make-hash-table :test #'equal)))
                                                       (setf (gethash source h) "hack")
                                                       h))))
    (language.c.cpp.evaluator:evaluate ast environment target)))

(defmethod parse ((source pathname) &rest args &key search-path)
  (let* ((preprocessed-input (with-output-to-string (stream)
                               (apply #'preprocess source :target stream args)))
         (builder            'list ; (make-instance 'language.c.)
                             )
         (ast                (language.c.c.parser:parse
                              preprocessed-input builder)))
    ast))
