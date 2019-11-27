;;;; grammar.lisp --- Tests for the grammar rules of the cpp.parser module
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.cpp.parser.test)

(def-suite* :language.c.cpp.parser.grammar
  :in :language.c.cpp.parser)

(define-rule-test group
  ("typedef __gnuc_va_list va_list;~%#   define _VA_LIST_DEFINED~%"
   '(:group
     (:token (((:identifier ))))
     :bounds (0 .30))))

(define-rule-test if-section
  ("#if foo~%#endif" '(:if
                       (:test (((:identifier () :name "foo" :bounds (4 . 7)))))
                       :kind :if :bounds (0 . 14)))
  ("#ifdef foo~%#define __GLIBC_USE(F) __GLIBC_USE_ ## F~%#else~%bar~%#endif /* foo */~%"
   '(:if
     (:test (())
      :then (())
      :else (()))
     :kind :ifdef :bounds (0 . 120))))

(parse "#if defined __USE_XOPEN || defined __USE_XOPEN2K8
# ifdef __GNUC__
#  ifndef _VA_LIST_DEFINED
typedef __gnuc_va_list va_list;
#   define _VA_LIST_DEFINED
#  endif
# else
#  include <stdarg.h>
# endif
#endif
" 'list)

(define-rule-test constant-expression
  ("0 && 0"
   '(:binary-expression
     (:operand (((:constant
                  ()
                  :type      :integer
                  :value     0
                  :size      nil
                  :unsigned? nil
                  :bounds    (0 . 1)))
                ((:constant
                  ()
                  :type      :integer
                  :value     0
                  :size      nil
                  :unsigned? nil
                  :bounds    (5 . 6)))))
     :operator :&& :bounds (0 . 6)))
  ; ("0 ## 0 ( 0 )" "y")
  )


#+later (bp:with-builder ('list)
  (esrap:parse 'preprocessing-file  "#define __TIMER_T_TYPE		void *
#define __BLKSIZE_T_TYPE
"))

#+later (bp:with-builder ('list)
  (esrap:parse 'preprocessing-file "# define __GNUC_PREREQ(maj, min) \\
	((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))"))
