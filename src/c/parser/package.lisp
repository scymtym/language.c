;;;; package.lisp --- Package definition for the c.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.c.parser
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:esrap
   #:defrule #:&bounds
   #:character-ranges
   #:? #:&)

  (:import-from #:language.c.shared.parser
   #:deftokens

   #:define-bracket-rule
   #:define-separator-list-rule

   #:identifier-list
   #:identifier

   #:constant
   #:integer-constant
   #:string-literal

   #:punctuator-[   #:punctuator-]   #:punctuator-|(| #:punctuator-|)|
   #:punctuator-{   #:punctuator-}   #:punctuator-|.| #:punctuator-->

   #:punctuator-++  #:punctuator-&
   #:punctuator-*   #:punctuator-+   #:punctuator--   #:punctuator-~   #:punctuator-!

   #:punctuator-|,| #:punctuator-|;| #:punctuator-|:|

   #:conditional-expression
   #:constant-expression)

  ;; Protocol
  (:export
   #:parse)

  ;; Rules
  (:export
   ;; Declarations
   #:brace-initializer
   #:initializer-list

   ;; Statements
   #:while-statement
   #:do-statement
   #:for-statement

   #:goto-statement
   #:continue-statement

   ;; Translation unit and toplevel
   #:translation-unit

   #:external-declaration
   #:function-definition))
