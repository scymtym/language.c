;;;; package.lisp --- Package definition for the interface module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.interface
  (:use
   #:cl)

  (:export
   #:preprocess
   #:parse))
