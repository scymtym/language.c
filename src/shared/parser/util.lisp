;;;; utilities.lisp --- Utilities shared between C18 and the preprocessor language
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;; Copyright (C) 2019 Daniel Kochmański
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;         Daniel Kochmański

(cl:in-package #:language.c.shared.parser)

(defmacro deftokens ((name resultp &key (whitespace          '(* whitespace))
                                        requires-separation?)
                     &body symbols)
  `(progn
     (defrule ,name (or ,@symbols))
     ,@(mapcar (lambda (token-name)
                 (let ((rule-name (symbolicate name '#:- token-name))
                       (result    (if resultp
                                      (make-keyword (string-upcase token-name))
                                      nil))
                       (string    (string token-name)))
                   `(defrule ,rule-name
                        (and ,whitespace
                             ,string
                             ,@(when requires-separation?
                                 '((esrap:! (or identifier-nondigit digit))))
                             ,whitespace)
                      (:constant ,result))))
               symbols)))

(defmacro define-bracket-rule (name (open close) expression &body options)
  `(defrule ,name
       (and ,open ,expression ,close)
     (:function second)
     ,@options))

(defmacro define-separator-list-rule
    (name element-expression separator-expression)
  (let ((rest-name (symbolicate name '#:-rest)))
   `(progn
      (defrule ,name
          (and ,element-expression (* ,rest-name))
        (:destructure (first second)
          (list* first second)))

      (defrule ,rest-name
          (and ,separator-expression ,element-expression)
        (:function second)))))
