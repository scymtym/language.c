;;;; package.lisp --- Package definition for the preprocessor.evaluator module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.preprocessor.evaluator
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:model #:language.c.preprocessor.model))

  ;; Environment protocol
  (:export
   #:entries
   #:lookup
   #:resolve-include)

  ;; Environment classes
  (:export
   #:environment

   #:search-path-environment
   #:search-path

   #:file-environment
   #:file

   #:include-environment)

  ;; Evaluation protocol
  (:export
   #:evaluate))
