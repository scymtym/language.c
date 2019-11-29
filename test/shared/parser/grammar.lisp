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

(define-rule-test (floating-constant :floating-constants? t)
  ("1.0"   '(:constant
             ()
             :type :floating :size nil :value 1 :bounds (0 . 3)))
  ("1.5"   '(:constant
             ()
             :type :floating :size nil :value 3/2 :bounds (0 . 3)))
  ("1.5e2" '(:constant
             ()
             :type :floating :size nil :value 150 :bounds (0 . 5))))

(test rule.constant-expression

  (let ((language.c.shared.parser::*floating-point-constants?* t))
    (architecture.builder-protocol:with-builder ('list)
      (finishes (esrap:parse 'constant-expression "( 0 /*hi*/ || ( 0 && + 0.1 >= 201112L ) )"))))

  (let ((language.c.shared.parser::*floating-point-constants?* t))
    (architecture.builder-protocol:with-builder ('list)
      (esrap:parse 'constant-expression "('foo' == 0x0.1p1)"))))
