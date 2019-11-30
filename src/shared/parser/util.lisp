;;;; utilities.lisp --- Utilities shared between C18 and the preprocessor language
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;; Copyright (C) 2019 Daniel Kochmański
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;         Daniel Kochmański

(cl:in-package #:language.c.shared.parser)

(defun disambiguate (string other-strings)
  (if-let ((confusable (find-if (lambda (other-string)
                                  (and (not (eq string other-string))
                                       (starts-with-subseq string other-string)))
                                other-strings)))
    `(and ,string (! ,(subseq confusable (length string))))
    string))

(defmacro deftokens ((name resultp &key (skippable          'skippable*)
                                        requires-separation?)
                     &body symbols)
  (let ((strings    (map 'list #'string symbols))
        (rule-names '()))
    `(progn
       ,@(mapcar (lambda (token-string token-name)
                   (let ((rule-name (symbolicate name '#:- token-name))
                         (result    (if resultp
                                        (make-keyword (string-upcase token-name))
                                        nil))
                         (string    (disambiguate token-string strings)))
                     (push rule-name rule-names)
                     `(defrule ,rule-name
                          (and ,skippable
                               ,string
                               ,@(when requires-separation?
                                   '((esrap:! (or identifier-nondigit digit))))
                               ,skippable)
                        (:constant ,result))))
                 strings symbols)
       (defrule ,name (or ,@(reverse rule-names))))))

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
