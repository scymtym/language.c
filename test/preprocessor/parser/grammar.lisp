;;;; grammar.lisp --- Tests for the grammar rules of the preprocessor.parser module
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.preprocessor.parser.test)

(def-suite* :language.c.preprocessor.parser.grammar
  :in :language.c.preprocessor.parser)

(define-rule-test group
  ("typedef __gnuc_va_list va_list;~%#   define _VA_LIST_DEFINED~%"
   '(:group
     (:part (((:line
               (:token (((:identifier () :name "typedef"        :bounds ( 0 .  7)))
                        ((:identifier () :name "__gnuc_va_list" :bounds ( 8 . 22)))
                        ((:identifier () :name "va_list"        :bounds (23 . 30)))
                        ((:punctuator () :which :|;|            :bounds (30 . 31)))))
               :bounds (0 . 32)))
             ((:define-object-like-macro
               (:name (((:identifier () :name "_VA_LIST_DEFINED" :bounds (43 . 59)))))
               :bounds (36 . 59)))))
     :bounds (0 . 60))))

;;; If

(define-rule-test if-section
  ("#ifdef 1~%#endif"       nil)
  ("#ifdef foo.bar~%#endif" nil)
  ("#ifndef 1~%#endif"      nil)


  ("#if foo~%#endif"  '(:if
                        (:test (((:identifier () :name "foo" :bounds (4 . 7)))))
                        :kind :if :bounds (0 . 14)))
  ("#ifdef foo~%#define __GLIBC_USE(F) __GLIBC_USE_ ## F~%#else~%bar~%#endif /* foo */~%"
   '(:if
     (:test (((:identifier () :name "foo" :bounds (7 . 10))))
      :then (())
      :else (()))
     :kind :ifdef :bounds (0 . 79))))

(test rule.if-section.2

  (finishes
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
"
           'list :rule 'if-section))

  (finishes
   (parse "#ifdef __cplusplus
# define __END_DECLS foo
#else
# define __END_DECLS bar
#endif
"
          'list :rule 'if-section))

  (finishes
    (parse "#if foo
#endif // foo
" 'list :rule 'if-section)))

;;; Control lines

(define-rule-test language.c.preprocessor.parser::include-line
  ("include <name>//comment"
   '(:include
     (:filename (((:header-name () :kind :system :name "name" :bounds (8 . 14)))))
     :bounds (0 . 14))
   14))

(define-rule-test define-identifier-line
  ("define"         nil)
  ("define 1"       nil)

  ("define foo"
   '(:define-object-like-macro
     (:name (((:identifier () :name "foo" :bounds (7 . 10)))))
     :bounds (0 . 10)))
  ("define foo 1"
   '(:define-object-like-macro
     (:replacement (((:number     () :value "1"  :bounds (11 . 12))))
      :name        (((:identifier () :name "foo" :bounds ( 7 . 10)))))
     :bounds (0 . 12)))

  ("define foo()"
   '(:define-function-like-macro
     (:name (((:identifier () :name "foo" :bounds (7 . 10)))))
     :ellipsis? nil :bounds (0 . 12)))
  ("define foo() x"
   '(:define-function-like-macro
     (:replacement (((:identifier () :name "x"   :bounds (13 . 14))))
      :name        (((:identifier () :name "foo" :bounds ( 7 . 10)))))
     :ellipsis? nil :bounds (0 . 14)))
  ("define foo(...)"
   '(:define-function-like-macro
     (:name (((:identifier () :name "foo" :bounds (7 . 10)))))
     :ellipsis? t :bounds (0 . 15)))
  ("define foo(x,y)"
   '(:define-function-like-macro
     (:parameter (((:identifier () :name "x"   :bounds (11 . 12)))
                  ((:identifier () :name "y"   :bounds (13 . 14))))
      :name      (((:identifier () :name "foo" :bounds (7 . 10)))))
     :ellipsis? nil :bounds (0 . 15)))
  ("define foo(x,...)"
   '(:define-function-like-macro
     (:parameter (((:identifier () :name "x"   :bounds (11 . 12))))
      :name      (((:identifier () :name "foo" :bounds (7 . 10)))))
     :ellipsis? t :bounds (0 . 17))))

;;;

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


(test rule.preprocessing-file

  (finishes
    (parse "#define __TIMER_T_TYPE		void *
#define __BLKSIZE_T_TYPE
" 'list :rule 'preprocessing-file))

  (finishes
    (parse "# define __GNUC_PREREQ(maj, min) \\
	((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))"
           'list :rule 'preprocessing-file))

  (finishes
    (parse "# if (defined __cplusplus						\\
      || (defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L))
#  define __inline	inline
# else
#  define __inline		/* No inline functions.  */
# endif
"
           'list :rule 'preprocessing-file))

  (finishes
    (parse "#define __ptr_t void *

1"
           'list :rule 'preprocessing-file)))
