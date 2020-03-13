;;;; package.lisp --- Package definition for the interface module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.interface
  (:use
   #:cl)

  (:local-nicknames
   (#:eval #:language.c.preprocessor.evaluator)
   (#:info #:language.c.toolchain-information))

  (:export
   #:preprocess
   #:parse
   #:repl))
