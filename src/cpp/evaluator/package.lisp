;;;; package.lisp --- Package definition for the cpp.evaluator module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.cpp.evaluator
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:model #:language.c.cpp.model))

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
