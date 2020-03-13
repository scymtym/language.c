;;;; package.lisp --- Package definition for the toolchain-information module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.toolchain-information
  (:use
   #:cl)

  (:local-nicknames
   (#:eval #:language.c.preprocessor.evaluator))

  ;; Toolchain IDs
  (:export
   #:guess-toolchain
   #:make-toolchain-id
   #:guess-toolchain-id)

  ;; Toolchain information registry
  (:export
   #:find-information
   #:register-information)

  ;; Environment augmentation 
  (:export
   #:augment-environment!))
