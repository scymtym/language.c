;;;; package.lisp --- Package definition for the cpp.model module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.c.cpp.model
  (:use
   #:cl)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:export
   #:identifier
   #:name

   #:number*
   #:value

   #:group
   #:parts

   #:line
   #:tokens

   #:if*
   #:test
   #:then
   #:else
   #:macro
   #:object-like-macro
   #:function-like-macro)

  ;; Control line nodes
  (:export
   #:include #:filename

   #:macro #:name  #:replacement
   #:object-like-macro
   #:function-like-macro #:parameters

   #:undefine #:name

   #:line

   #:error* #:message

   #:pragma

   #:kind)

  ;; Builder
  (:export
   #:builder))
