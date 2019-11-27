;;;; package.lisp --- Package definition for the shared.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.shared.parser
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:esrap
   #:defrule #:&bounds
   #:character-ranges #:~
   #:? #:!)

  ;; Utilities
  (:export
   #:deftokens
   #:define-bracket-rule
   #:define-separator-list-rule)

  (:export
   #:identifier-list
   #:identifier)

  ;; Constant rules
  (:export
   #:constant
   #:integer-constant
   #:string-literal)

  ;; Punctuator rules
  (:export
   #:punctuator-[    #:punctuator-]      #:punctuator-|(| #:punctuator-|)|
   #:punctuator-{    #:punctuator-}      #:punctuator-|.| #:punctuator-->

   #:punctuator-/    #:punctuator-%      #:punctuator-<<  #:punctuator->>
   #:punctuator-<    #:punctuator->      #:punctuator-<=  #:punctuator->=
   #:punctuator-==   #:punctuator-!=     #:punctuator-^   #:punctuator-&& #:punctuator-\|\|

   #:punctuator-++   #:punctuator---     #:punctuator-&   #:punctuator-\|
   #:punctuator-*    #:punctuator-+      #:punctuator--   #:punctuator-~ #:punctuator-!

   #:punctuator-?    #:punctuator-|:|    #:punctuator-|;| #:punctuator-|...|
   #:punctuator-=    #:punctuator-*=     #:punctuator-/=  #:punctuator-%=
   #:punctuator-+=   #:punctuator--=     #:punctuator-<<= #:punctuator->>=
   #:punctuator-&=   #:punctuator-\|=    #:punctuator-^=

   #:punctuator-|,|  #:punctuator-##     #:punctuator-#
   #:punctuator-|<:| #:punctuator-|:>|   #:punctuator-<% #:punctuator-%>
   #:punctuator-|%:| #:punctuator-|%:%:|)

  ;; Expression rules
  (:export
   #:condition-expression
   #:constant-expression))
