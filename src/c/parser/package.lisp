;;;; package.lisp --- Package definition for the c.parser module
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.c.parser
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:keyword
   #:declaration)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:esrap
   #:defrule #:&bounds
   #:character-ranges
   #:? #:& #:!)

  (:import-from #:parser.common-rules
   #:defrule/s)

  (:import-from #:language.c.shared.parser
   #:deftokens

   #:define-bracket-rule
   #:define-separator-list-rule

   #:whitespace #:whitespace+ #:whitespace*

   #:identifier-list
   #:identifier

   #:constant
   #:integer-constant
   #:string-literal

   #:punctuator-[   #:punctuator-]   #:punctuator-|(| #:punctuator-|)|
   #:punctuator-{   #:punctuator-}   #:punctuator-|.| #:punctuator-->

   #:punctuator-++  #:punctuator---  #:punctuator-&
   #:punctuator-*   #:punctuator-+   #:punctuator--   #:punctuator-~   #:punctuator-!

   #:punctuator-=

   #:punctuator-|:| #:punctuator-|;| #:punctuator-|...|
   #:punctuator-|,|

   #:conditional-expression
   #:constant-expression)

  ;; Protocol
  (:export
   #:parse)

  ;; Rules
  (:export
   ;; Declarations
   #:declaration
   #:enum-specifier

   #:brace-initializer
   #:initializer-list

   ;; Statements
   #:while-statement
   #:do-statement
   #:for-statement

   #:goto-statement
   #:continue-statement
   #:break-statement
   #:return-statement

   ;; Translation unit and toplevel
   #:translation-unit

   #:external-declaration
   #:function-definition))
