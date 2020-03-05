;;;; package.lisp --- Package definition for the preprocessor.model module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.preprocessor.model
  (:use
   #:cl)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  ;; Lexical elements
  (:export
   #:identifier #:name

   #:character-literal #:value

   #:number* #:value

   #:header-name #:kind #:name)

  ;;
  (:export
   #:group #:parts

   #:line #:tokens

   #:if* #:kind #:test #:then #:else)

  ;; Control line nodes
  (:export
   #:include #:filename

   #:define #:name  #:replacement
   #:define-object-like-macro
   #:define-function-like-macro #:parameters

   #:undefine #:name

   #:line

   #:error* #:message

   #:pragma #:tokens)

  ;; Builder
  (:export
   #:builder))
