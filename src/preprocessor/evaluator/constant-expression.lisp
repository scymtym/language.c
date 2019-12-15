;;;; constant-expression.lisp --- Evaluation of constant expressions.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.preprocessor.evaluator)

(defun bit-boolean (integer)
  (if (plusp integer) 1 0))

(defun eval-constant-expression (expression)
  (labels ((rec (node)
             (ecase (first node)
               (:constant
                (getf (cddr node) :value))
               (:unary-expression
                (let ((operator (getf (cddr node) :operator))
                      (operand  (first (first (getf (second node) :operand)))))
                  (ecase operator
                    (:!
                     (- 1 (bit-boolean (rec operand)))))))
               (:binary-expression
                (let ((operator (getf (cddr node) :operator))
                      (operands (map 'list #'first
                                     (getf (second node) :operand))))
                  (case operator
                    (:&&
                     (if (find 0 operands :key #'rec) 0 1))
                    (:\|\|
                     (if (find 1 operands :key #'rec) 1 0))
                    ((:+ :- :* :/ :<< :>>)
                     (apply (ecase operator
                              (:-  '-)
                              (:+  '+)
                              (:*  '*)
                              (:/  '/)
                              (:<< 'ash)
                              (:>> (lambda (value shift)
                                     (ash value (- shift)))))
                            (map 'list #'rec operands)))
                    (t
                     (if (apply (ecase operator
                                  (:<  '<)
                                  (:>  '>)
                                  (:<= '<=)
                                  (:>= '>=)
                                  (:== '=)
                                  (:!= '/=))
                                (map 'list #'rec operands))
                         1 0)))))
               (:ternary-expression
                (let ((operator1 (getf (cddr node) :operator1))
                      (operands  (map 'list #'first
                                      (getf (second node) :operand))))
                  (ecase operator1
                    (:? (rec  (if (plusp (rec (first operands)))
                                  (second operands)
                                  (third operands))))))))))
    (plusp (rec expression))))
