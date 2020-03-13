;;;; grammar.lisp --- Tests for the grammar rules of the c.parser module
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.c.parser.test)

(def-suite* :language.c.c.parser.grammar
  :in :language.c.c.parser)

;;; A.2.1 Expressions

(define-rule-test language.c.c.parser::postfix-expression
  ;; No suffix
  ("a"
   '(:identifier () :name "a" :bounds (0 . 1)))
  ;; Primary expression with suffix
  ("a[b]"
   '(:subscript
     (:index      (((:identifier () :name "b" :bounds (2 . 3))))
      :expression (((:identifier () :name "a" :bounds (0 . 1)))))
     :bounds (0 . 4)))
  ("a(b,c)"
   '(:call
     (:argument (((:identifier () :name "b" :bounds (2 . 3)))
                 ((:identifier () :name "c" :bounds (4 . 5))))
      :function (((:identifier () :name "a" :bounds (0 . 1)))))
     :bounds (0 . 6)))
  ("a.b"
   '(:member-access
     (:member     (((:identifier () :name "b" :bounds (2 . 3))))
      :expression (((:identifier () :name "a" :bounds (0 . 1)))))
     :pointer nil :bounds (0 . 3)))
  ("a->b"
   '(:member-access
     (:member     (((:identifier () :name "b" :bounds (3 . 4))))
      :expression (((:identifier () :name "a" :bounds (0 . 1)))))
     :pointer t :bounds (0 . 4)))
  ("a++"
   '(:unary-expression
     (:expression (((:identifier () :name "a" :bounds (0 . 1)))))
     :operator :|++| :position :postfix :bounds (0 . 3)))
  ("a--"
   '(:unary-expression
     (:expression (((:identifier () :name "a" :bounds (0 . 1)))))
     :operator :|--| :position :postfix :bounds (0 . 3)))

  ;; Type name and initializer with suffix
  ;; ("(int){a=1}" nil)
  )

(define-rule-test language.c.c.parser::unary-expression
  ("a"
   '(:identifier () :name "a" :bounds (0 . 1)))
  ("a++"
   '(:unary-expression
     (:expression (((:identifier () :name "a" :bounds (0 . 1)))))
     :operator :|++| :position :postfix :bounds (0 . 3)))
  ("++a"
   '(:unary-expression
     (:operand (((:identifier () :name "a" :bounds (2 . 3)))))
     :operator :++ :bounds (0 . 3)))
  ("--a"
   '(:unary-expression
     (:operand (((:identifier () :name "a" :bounds (2 . 3)))))
     :operator :-- :bounds (0 . 3)))
  ("-a"
   '(:unary-expression
     (:operand (((:identifier () :name "a" :bounds (1 . 2)))))
     :operator :- :bounds (0 . 2)))
  ("+a"
   '(:unary-expression
     (:operand (((:identifier () :name "a" :bounds (1 . 2)))))
     :operator :+ :bounds (0 . 2)))
  ("&a"
   '(:unary-expression
     (:operand (((:identifier () :name "a" :bounds (1 . 2)))))
     :operator :& :bounds (0 . 2)))
  ("sizeof(int)"
   '(:unary-expression
     (:operand (((:primitive-type () :which :int :bounds (7 . 10)))))
     :operator :sizeof :bounds (0 . 11)))
  ("_Alignof(int)"
   '(:unary-expression
     (:operand (((:primitive-type () :which :int :bounds (9 . 12)))))
     :operator :_alignof :bounds (0 . 13))))

;;; A.2.2 Declarations

(define-rule-test declaration
  ("int x;"
   '(:declaration))

  ("typedef int bar;"
   '(:declaration
     (:specifier ((:typedef)
                  ((:type-specifier () :which :int :bounds (8 . 12)))
                  ((:type-specifier () :which (:identifier nil :name "bar" :bounds (12 . 15))
                    :bounds (12 . 15)))))
     :bounds (0 . 16)))
  ("typedef foo bar;"
   '(:type-declaration
     (:type (((:type-specifier () :which (:identifier nil :name "foo" :bounds (8 . 11))
               :bounds (8 . 12))))
      :name (((:identifier nil :name "bar" :bounds (12 . 15)))))
     :bounds (0 . 16)))

  ("typedef struct _IO_FILE __FILE;"
   '(:type-declaration
     (:declarator (((:identifier () :name "__FILE" :bounds (24 . 30))))
      :type       (((:struct
                     (:name (((:identifier () :name "_IO_FILE" :bounds (15 . 23)))))
                     :bounds (8 . 23)))))
     :bounds (0 . 31)))
  ("typedef struct{int __val[2];} foo;" nil
   '(:type-declaration
     (:declarator (((:identifier () :name "foo" :bounds (24 . 30))))
      :type       (((:struct
                     ()
                     :bounds (8 . 23)))))
     :bounds (0 . 31)))

  ("typedef enum foo bar;"
   '(:type-declaration
     (:declarator (((:identifier () :name "bar" :bounds (17 . 20))))
      :type       (((:enum
                     (:name (((:identifier () :name "foo" :bounds (13 . 16)))))
                     :bounds (8 . 16)))))
     :bounds (0 . 21)))

  ("_Static_assert (foo, \"bla\");"
   '(:static-assert
     (:message    (((:string-literal () :value "bla" :encoding nil :bounds (21 . 26))))
      :expression (((:identifier () :name "foo" :bounds (16 . 19)))))
     :bounds (0 . 28))))

(define-rule-test enum-specifier
  ("enum"    nil)
  ("enum {}" nil)
  ("enum foo {}"
   '(:enum
     (:name (((:identifier () :name "foo" :bounds (5 . 8)))))
     :bounds (0 . 8))
   8 t)

  ("enum foo"
   '(:enum
     (:name (((:identifier () :name "foo" :bounds (5 . 8)))))
     :bounds (0 . 8)))
  ("enum {FOO}"
   '(:enum
     (:enumerator (((:enumerator
                     (:name (((:identifier () :name "FOO" :bounds (6 . 9)))))
                     :bounds (6 . 9)))))
     :bounds (0 . 10)))
  ("enum foo {~%FOO~%}"
   '(:enum
     (:enumerator (((:enumerator
                     (:name (((:identifier () :name "FOO" :bounds (11 . 14)))))
                     :bounds (11 . 14))))
      :name       (((:identifier () :name "foo" :bounds (5 . 8)))))
     :bounds (0 . 16)))
  ("enum foo {~%FOO,BAR~%}"
   '(:enum
     (:enumerator (((:enumerator
                     (:name (((:identifier () :name "FOO" :bounds (11 . 14)))))
                     :bounds (11 . 14)))
                   ((:enumerator
                     (:name (((:identifier () :name "BAR" :bounds (15 . 18)))))
                     :bounds (15 . 18))))
      :name       (((:identifier () :name "foo" :bounds (5 . 8)))))
     :bounds (0 . 20))))

(define-rule-test declarator
  ("*const**" '(:declarator)))

(define-rule-test brace-initializer
  ("{ .foo = 1 }"
   '(:brace-initializer
     (:initializer (((:initializer
                      (:expression  (((:constant
                                       ()
                                       :type      :integer
                                       :value     1
                                       :size      nil
                                       :unsigned? nil
                                       :bounds    (9 . 10))))
                       :designation (((:member-designator
                                       (:member (((:identifier
                                                   ()
                                                   :name   "foo"
                                                   :bounds (3 . 6)))))
                                       :bounds (2 . 6)))))
                      :bounds (2 . 10)))))
     :comma  nil
     :bounds (0 . 12)))

  ("{ .foo.bar = 1 }"
   '(:brace-initializer
     (:initializer (((:initializer
                      (:expression  (((:constant
                                       ()
                                       :type      :integer
                                       :value     1
                                       :size      nil
                                       :unsigned? nil
                                       :bounds    (13 . 14))))
                       :designation (((:member-designator
                                       (:member (((:identifier
                                                   ()
                                                   :name   "foo"
                                                   :bounds (3 . 6)))))
                                       :bounds (2 . 6)))
                                     ((:member-designator
                                       (:member (((:identifier
                                                   ()
                                                   :name   "bar"
                                                   :bounds (7 .  10)))))
                                       :bounds (6 . 10)))))
                      :bounds (2 . 14)))))
     :comma  nil
     :bounds (0 . 16)))
  #+no ("{ .foo = { 1 }, [1] = 1 }"
        '(:brace-initializer
          (:initializer (((:member-designator ()
                           :member
                           :bounds (3 . 6)))))
          :comma  nil
          :bounds (0 . 25)))

  )

(define-rule-test initializer-list
  (".foo = { 1 }"
   '(:initializer
     (:expression)
     :bounds (0 . 12))))

;; (esrap:parse 'declaration-specifiers "extern static")
;;
;; (bp:with-builder ('list)
;;   (esrap:parse 'declarator "int(a, b)"))

;; (parse "(const void*__s1,const void*__)" 'list :rule 'parameter-type-list)
;; (parse "void*restrict __dest" 'list :rule 'parameter-declaration)
;; (parse "const void*__dest" 'list :rule 'parameter-declaration)
;; (parse "extern int memcmp(const void*__s1,const void*__)" 'list :rule 'declaration)

;; TODO integrate
#+TODO (progn
  (parse "int i =10;" 'list :rule 'declaration)

  (parse "struct timeval
{__time_t tv_sec;
__suseconds_t tv_usec;};" 'list
:rule 'translation-unit)

  (parse "struct _fpxreg _st[8];" 'list
         :rule 'language.c.c.parser::struct-declaration/declarator)

  (parse "const union sigval __val" 'list
         :rule 'language.c.c.parser::parameter-declaration)
  (parse  'list
         :rule 'function-declaration)
  (parse  'list
         :rule 'function-declaration)

  (parse "return((__uint16_t)((((__bsx)>>8)&0xff)|(((__bsx)&0xff)<<8)))" 'list
         :rule 'return-statement)

  (parse "((__uint16_t)((((__bsx)>>8)&0xff)|(((__bsx)&0xff)<<8)))" 'list
         :rule 'expression/parentheses)

  (parse "((__uint16_t)1)" 'list :rule 'expression/parentheses)
  (parse "(__uint16_t)1" 'list :rule 'cast-expression)

  (parse "((((__bsx)>>8)&0xff)|(((__bsx)&0xff)<<8))" 'list :rule 'cast-expression)
  (parse "(((__bsx)>>8)&0xff)" 'list :rule 'cast-expression)
  (parse "(((__bsx)&0xff)<<8)" 'list :rule 'expression/parentheses))

(define-rule-test language.c.c.parser::function-declaration
  ;; Real-world cases
  ("extern int memcmp(const void*__s1,const void*__);"
   '(:function-declaration
     ))
  ("extern int sigqueue(__pid_t __pid,int __sig,const union sigval __val);"
   '(:function-declaration
     ))
  ("extern unsigned int alarm(unsigned int __seconds);"
   '(:function-declaration
     )))

;;; A.2.3 Statements

(define-rule-test for-statement
  ("for (;;) {}"
   '(:for-statement
     (:body (((:compound-statement () :bounds (9 . 11)))))
     :bounds (0 . 11)))

  ("for (int x = 1; x < 10; ++x) {}"
   '(:for-statement
     (:body (((:compound-statement () :bounds (29 . 31))))
      :step (((:unary-expression
               (:operand (((:identifier () :name "x" :bounds (26 . 27)))))
               :operator :++ :bounds (24 . 27))))
      :test (((:binary-expression
               (:operand (((:identifier () :name "x" :bounds (16 . 17)))
                          ((:constant
                            ()
                            :type      :integer
                            :value     10
                            :size      nil
                            :unsigned? nil
                            :bounds    (20 . 22)))))
               :operator :< :bounds (16 . 22))))
      :init (((:variable-declaration
               (:declarator (((:init-declarator
                               (:initializer (((:constant
                                                ()
                                                :type      :integer
                                                :value     1
                                                :size      nil
                                                :unsigned? nil
                                                :bounds    (13 . 14))))
                                :name (((:identifier () :name "x" :bounds (9 . 10)))))
                               :bounds (9 . 14))))
                :type       (((:primitive-type
                               ()
                               :which  (nil (nil :int))
                               :bounds (5 . 9)))))
               :bounds (5 . 16)))))
     :bounds (0 . 31))))

(define-rule-test goto-statement
  ("goto foo"
   '(:goto-statement
     (:label (((:identifier () :name "foo" :bounds (5 . 8)))))
     :bounds (0 . 8)))
  ("goto /* gogogo */ foo"
   '(:goto-statement
     (:label (((:identifier () :name "foo" :bounds (18 . 21)))))
     :bounds (0 . 21))))

(define-rule-test continue-statement
  ("continue" '(:continue-statement () :bounds (0 . 8))))

(define-rule-test break-statement
  ("break" '(:break-statement () :bounds (0 . 5))))

(define-rule-test return-statement
  ("return"   '(:return-statement () :bounds (0 . 6)))
  ("return 1" '(:return-statement
                (:value (((:constant
                           ()
                           :type      :integer
                           :value     1
                           :size      nil
                           :unsigned? nil
                           :bounds    (7 . 8)))))
                :bounds (0 . 8))))

;;; A.2.4 External definitions

(define-rule-test translation-unit
    ("extern void main(int argc, char* argv[]) {~%return 0;~%}"
     '(:translation-unit)))

(define-rule-test function-definition
  ("int f() {}"
   '(:function-definition
     (:return    ((:int))
      :name      (((:identifier () :name "f" :bounds (4 . 5)))))
     :bounds (0 . 10)))

  ("typedef int fp(void);"
   '(:function-definition))

  ("int(*fp)(void);"
   '(:function-definition))

  ("int~@
    a~@
    (~@
    /*foo */~@
    int x) { return 1; }"
   '(:function-definition))

  ("int *const*foo(int x) {}"
   '(:function-definition)))
