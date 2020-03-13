;;;; util.lisp --- Utilities for the C18 grammar.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.c.parser)

(defun maybe-apply-suffix (primary suffixes start)
  (if suffixes
      (reduce (lambda (suffixes expression)
                (funcall suffixes expression start))
              (reverse suffixes)
              :initial-value primary :from-end t) ; TODO avoid reverse))
      primary))
