;;;; package.lisp --- Package definition for the preprocessor.parser module
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.preprocessor.parser
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:keyword)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:esrap
   #:defrule #:&bounds
   #:character-ranges #:? #:& #:! #:~)

  (:import-from #:parser.common-rules
   #:<end-of-input>)

  (:import-from #:language.c.shared.parser
   #:deftokens

   #:whitespace
   #:whitespace/same-line #:whitespace/same-line+ #:whitespace/same-line*
   #:comment

   #:digit
   #:identifier-nondigit
   #:punctuator-|.| #:sign

   #:punctuator-|(| #:punctuator-|)|

   #:punctuator-|...|

   #:punctuator-|,| #:punctuator-# #:punctuator-## ; TODO why is this shared?

   #:identifier-list
   #:identifier

   #:character-constant
   #:string-literal
   #:punctuator

   #:constant-expression)

  (:export
   #:parse)

  (:export
   #:preprocessing-file

   #:group

   #:if-section

   #:define-identifier-line

   #:constant-expression))
