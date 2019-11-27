;;;; package.lisp --- Package definition for the cpp.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.cpp.parser
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:esrap
   #:defrule #:&bounds
   #:character-ranges #:? #:& #:! #:~)

  (:import-from #:parser.common-rules
   #:<end-of-input>)

  (:import-from #:language.c.shared.parser
   #:deftokens

   #:whitespace #:whitespace/same-line
   #:comment

   #:digit
   #:identifier-nondigit
   #:punctuator-|.| #:sign

   #:punctuator-|(| #:punctuator-|)|
   #:punctuator-|#| ; TODO why is this shared?
   ; #:|if| #:|else|

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

   #:constant-expression))
