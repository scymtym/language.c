;;;; variables.lisp --- Variables used in the shared.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.shared.parser)

(deftype skippable-mode ()
  '(member :whitespace :whitespace/same-line))
(declaim (type skippable-mode *skippable-mode*))
(defvar *skippable-mode* :whitespace/same-line)

(defvar *floating-point-constants?* nil)

(defvar *extended-unary-expression* nil)
