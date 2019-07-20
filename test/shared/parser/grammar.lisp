;;;; grammar.lisp --- Tests for the grammar rules of the c.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.shared.parser.test)

(def-suite* :language.c.shared.parser.grammar
  :in :language.c.shared.parser)

;;; Constants

(define-rule-test integer-constant
  ("1"   '(:constant
           ()
           :type :integer :value 1 :size nil   :unsigned? nil       :bounds (0 . 1)))
  ;; Suffixes
  ("1u"  '(:constant
           ()
           :type :integer :value 1 :size nil   :unsigned? :unsigned :bounds (0 . 2)))
  ("1ul" '(:constant
           ()
           :type :integer :value 1 :size :long :unsigned? :unsigned :bounds (0 . 3))))
