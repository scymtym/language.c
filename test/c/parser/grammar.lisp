;;;; grammar.lisp --- Tests for the grammar rules of the c.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.c.parser.test)

(def-suite* :language.c.c.parser.grammar
  :in :language.c.c.parser)

;;; A.2.2 Declarations

(define-rule-test declaration
  ("int x;"
   '(:declaration)))

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

;;; A.2.3 Statements

(define-rule-test for-statement
  ("for (;;) {}"
   '(:for-statement
     ()
     :bounds (0 . 11)))
  ("for (int x = 1; x < 10; ++x) {}"
   '(:for-statement
     (:step ()
      :test ()
      :init ())
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

;; (esrap:parse 'declaration-specifiers "extern static")
;;
;; (bp:with-builder ('list)
;;   (esrap:parse 'declarator "int(a, b)"))

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
  ("int
a
(
/*foo */
int x) { return 1; }"
   '(:function-definition))
  ("int *const*foo(int x) {}"
   '(:function-definition)))
