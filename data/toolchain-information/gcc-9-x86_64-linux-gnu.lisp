
(LANGUAGE.C.TOOLCHAIN-INFORMATION:REGISTER-INFORMATION
 '#A((22) BASE-CHAR . "gcc-9-x86_64-linux-gnu")
 '(#P"/usr/lib/gcc/x86_64-linux-gnu/9/include/" #P"/usr/local/include/"
   #P"/usr/lib/gcc/x86_64-linux-gnu/9/include-fixed/"
   #P"/usr/include/x86_64-linux-gnu/" #P"/usr/include/")
 (LAMBDA (#1=#:ADD-MACRO705)
   (DECLARE (TYPE FUNCTION #1#))
   (FUNCALL #1# "_LP64"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2=#:BUILDER706
                     . #3=((MAKE-INSTANCE
                            'LANGUAGE.C.PREPROCESSOR.MODEL:BUILDER))))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "_STDC_PREDEF_H"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__ATOMIC_ACQUIRE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__ATOMIC_ACQ_REL"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4")))))))
   (FUNCALL #1# "__ATOMIC_CONSUME"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__ATOMIC_HLE_ACQUIRE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "65536")))))))
   (FUNCALL #1# "__ATOMIC_HLE_RELEASE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "131072")))))))
   (FUNCALL #1# "__ATOMIC_RELAXED"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0")))))))
   (FUNCALL #1# "__ATOMIC_RELEASE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "3")))))))
   (FUNCALL #1# "__ATOMIC_SEQ_CST"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "5")))))))
   (FUNCALL #1# "__BIGGEST_ALIGNMENT__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16")))))))
   (FUNCALL #1# "__BYTE_ORDER__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "__ORDER_LITTLE_ENDIAN__")))))))
   (FUNCALL #1# "__CHAR16_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "short"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__CHAR32_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__CHAR_BIT__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__DBL_DECIMAL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "17")))))))
   (FUNCALL #1# "__DBL_DENORM_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "double"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4.94065645841246544176568792868221372e-324L"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__DBL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "15")))))))
   (FUNCALL #1# "__DBL_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "double"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2.22044604925031308084726333618164062e-16L"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__DBL_HAS_DENORM__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__DBL_HAS_INFINITY__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__DBL_HAS_QUIET_NAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__DBL_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "53")))))))
   (FUNCALL #1# "__DBL_MAX_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "308")))))))
   (FUNCALL #1# "__DBL_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1024")))))))
   (FUNCALL #1# "__DBL_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "double"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.79769313486231570814527423731704357e"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :+))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "308L"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__DBL_MIN_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "307"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__DBL_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1021"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__DBL_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "double"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2.22507385850720138309023271733240406e-308L"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__DEC128_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1e-33DL")))))))
   (FUNCALL #1# "__DEC128_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "34")))))))
   (FUNCALL #1# "__DEC128_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "6145")))))))
   (FUNCALL #1# "__DEC128_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "9.999999999999999999999999999999999E6144DL")))))))
   (FUNCALL #1# "__DEC128_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "6142"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__DEC128_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1e-6143DL")))))))
   (FUNCALL #1# "__DEC128_SUBNORMAL_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0.000000000000000000000000000000001e-6143DL")))))))
   (FUNCALL #1# "__DEC32_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1e-6DF")))))))
   (FUNCALL #1# "__DEC32_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "7")))))))
   (FUNCALL #1# "__DEC32_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "97")))))))
   (FUNCALL #1# "__DEC32_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "9.999999E96DF")))))))
   (FUNCALL #1# "__DEC32_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE "94"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__DEC32_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1e-95DF")))))))
   (FUNCALL #1# "__DEC32_SUBNORMAL_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0.000001e-95DF")))))))
   (FUNCALL #1# "__DEC64_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1e-15DD")))))))
   (FUNCALL #1# "__DEC64_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16")))))))
   (FUNCALL #1# "__DEC64_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "385")))))))
   (FUNCALL #1# "__DEC64_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "9.999999999999999E384DD")))))))
   (FUNCALL #1# "__DEC64_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "382"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__DEC64_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1e-383DD")))))))
   (FUNCALL #1# "__DEC64_SUBNORMAL_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0.000000000000001e-383DD")))))))
   (FUNCALL #1# "__DECIMAL_BID_FORMAT__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__DECIMAL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "21")))))))
   (FUNCALL #1# "__DEC_EVAL_METHOD__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__ELF__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FINITE_MATH_ONLY__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0")))))))
   (FUNCALL #1# "__FLOAT_WORD_ORDER__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "__ORDER_LITTLE_ENDIAN__")))))))
   (FUNCALL #1# "__FLT128_DECIMAL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "36")))))))
   (FUNCALL #1# "__FLT128_DENORM_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "6.47517511943802511092443895822764655e-4966F128")))))))
   (FUNCALL #1# "__FLT128_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "33")))))))
   (FUNCALL #1# "__FLT128_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.92592994438723585305597794258492732e-34F128")))))))
   (FUNCALL #1# "__FLT128_HAS_DENORM__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT128_HAS_INFINITY__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT128_HAS_QUIET_NAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT128_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "113")))))))
   (FUNCALL #1# "__FLT128_MAX_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4932")))))))
   (FUNCALL #1# "__FLT128_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16384")))))))
   (FUNCALL #1# "__FLT128_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.18973149535723176508575932662800702e"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :+))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4932F128")))))))
   (FUNCALL #1# "__FLT128_MIN_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4931"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT128_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16381"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT128_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "3.36210314311209350626267781732175260e-4932F128")))))))
   (FUNCALL #1# "__FLT32X_DECIMAL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "17")))))))
   (FUNCALL #1# "__FLT32X_DENORM_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4.94065645841246544176568792868221372e-324F32x")))))))
   (FUNCALL #1# "__FLT32X_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "15")))))))
   (FUNCALL #1# "__FLT32X_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2.22044604925031308084726333618164062e-16F32x")))))))
   (FUNCALL #1# "__FLT32X_HAS_DENORM__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT32X_HAS_INFINITY__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT32X_HAS_QUIET_NAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT32X_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "53")))))))
   (FUNCALL #1# "__FLT32X_MAX_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "308")))))))
   (FUNCALL #1# "__FLT32X_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1024")))))))
   (FUNCALL #1# "__FLT32X_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.79769313486231570814527423731704357e"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :+))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "308F32x")))))))
   (FUNCALL #1# "__FLT32X_MIN_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "307"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT32X_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1021"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT32X_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2.22507385850720138309023271733240406e-308F32x")))))))
   (FUNCALL #1# "__FLT32_DECIMAL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "9")))))))
   (FUNCALL #1# "__FLT32_DENORM_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.40129846432481707092372958328991613e-45F32")))))))
   (FUNCALL #1# "__FLT32_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "6")))))))
   (FUNCALL #1# "__FLT32_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.19209289550781250000000000000000000e-7F32")))))))
   (FUNCALL #1# "__FLT32_HAS_DENORM__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT32_HAS_INFINITY__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT32_HAS_QUIET_NAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT32_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "24")))))))
   (FUNCALL #1# "__FLT32_MAX_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "38")))))))
   (FUNCALL #1# "__FLT32_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "128")))))))
   (FUNCALL #1# "__FLT32_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "3.40282346638528859811704183484516925e"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :+))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "38F32")))))))
   (FUNCALL #1# "__FLT32_MIN_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE "37"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT32_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "125"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT32_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.17549435082228750796873653722224568e-38F32")))))))
   (FUNCALL #1# "__FLT64X_DECIMAL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "21")))))))
   (FUNCALL #1# "__FLT64X_DENORM_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "3.64519953188247460252840593361941982e-4951F64x")))))))
   (FUNCALL #1# "__FLT64X_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "18")))))))
   (FUNCALL #1# "__FLT64X_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.08420217248550443400745280086994171e-19F64x")))))))
   (FUNCALL #1# "__FLT64X_HAS_DENORM__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT64X_HAS_INFINITY__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT64X_HAS_QUIET_NAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT64X_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__FLT64X_MAX_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4932")))))))
   (FUNCALL #1# "__FLT64X_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16384")))))))
   (FUNCALL #1# "__FLT64X_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.18973149535723176502126385303097021e"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :+))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4932F64x")))))))
   (FUNCALL #1# "__FLT64X_MIN_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4931"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT64X_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16381"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT64X_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "3.36210314311209350626267781732175260e-4932F64x")))))))
   (FUNCALL #1# "__FLT64_DECIMAL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "17")))))))
   (FUNCALL #1# "__FLT64_DENORM_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4.94065645841246544176568792868221372e-324F64")))))))
   (FUNCALL #1# "__FLT64_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "15")))))))
   (FUNCALL #1# "__FLT64_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2.22044604925031308084726333618164062e-16F64")))))))
   (FUNCALL #1# "__FLT64_HAS_DENORM__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT64_HAS_INFINITY__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT64_HAS_QUIET_NAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT64_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "53")))))))
   (FUNCALL #1# "__FLT64_MAX_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "308")))))))
   (FUNCALL #1# "__FLT64_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1024")))))))
   (FUNCALL #1# "__FLT64_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.79769313486231570814527423731704357e"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :+))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "308F64")))))))
   (FUNCALL #1# "__FLT64_MIN_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "307"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT64_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1021"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT64_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2.22507385850720138309023271733240406e-308F64")))))))
   (FUNCALL #1# "__FLT_DECIMAL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "9")))))))
   (FUNCALL #1# "__FLT_DENORM_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.40129846432481707092372958328991613e-45F")))))))
   (FUNCALL #1# "__FLT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "6")))))))
   (FUNCALL #1# "__FLT_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.19209289550781250000000000000000000e-7F")))))))
   (FUNCALL #1# "__FLT_EVAL_METHOD_TS_18661_3__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0")))))))
   (FUNCALL #1# "__FLT_EVAL_METHOD__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0")))))))
   (FUNCALL #1# "__FLT_HAS_DENORM__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT_HAS_INFINITY__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT_HAS_QUIET_NAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__FLT_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "24")))))))
   (FUNCALL #1# "__FLT_MAX_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "38")))))))
   (FUNCALL #1# "__FLT_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "128")))))))
   (FUNCALL #1# "__FLT_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "3.40282346638528859811704183484516925e"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :+))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "38F")))))))
   (FUNCALL #1# "__FLT_MIN_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE "37"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "125"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__FLT_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.17549435082228750796873653722224568e-38F")))))))
   (FUNCALL #1# "__FLT_RADIX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__FXSR__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GCC_ASM_FLAG_OUTPUTS__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GCC_ATOMIC_BOOL_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_ATOMIC_CHAR16_T_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_ATOMIC_CHAR32_T_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_ATOMIC_CHAR_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_ATOMIC_INT_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_ATOMIC_LLONG_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_ATOMIC_LONG_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_ATOMIC_POINTER_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_ATOMIC_SHORT_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_ATOMIC_TEST_AND_SET_TRUEVAL"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GCC_ATOMIC_WCHAR_T_LOCK_FREE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_HAVE_DWARF2_CFI_ASM"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GCC_IEC_559"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GCC_IEC_559_COMPLEX"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__GNUC_PATCHLEVEL__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GNUC_STDC_INLINE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__GXX_ABI_VERSION"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1013")))))))
   (FUNCALL #1# "__HAVE_SPECULATION_SAFE_VALUE"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__INT16_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "c")))))))
   (FUNCALL #1# "__INT16_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fff")))))))
   (FUNCALL #1# "__INT16_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "short"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INT32_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "c")))))))
   (FUNCALL #1# "__INT32_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffff")))))))
   (FUNCALL #1# "__INT32_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INT64_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :CONCATENATION)
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "c")))
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "L")))))))))
   (FUNCALL #1# "__INT64_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffL")))))))
   (FUNCALL #1# "__INT64_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INT8_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "c")))))))
   (FUNCALL #1# "__INT8_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7f")))))))
   (FUNCALL #1# "__INT8_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "signed"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "char")))))))
   (FUNCALL #1# "__INTMAX_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :CONCATENATION)
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "c")))
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "L")))))))))
   (FUNCALL #1# "__INTMAX_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffL")))))))
   (FUNCALL #1# "__INTMAX_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INTMAX_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__INTPTR_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffL")))))))
   (FUNCALL #1# "__INTPTR_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INTPTR_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__INT_FAST16_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffL")))))))
   (FUNCALL #1# "__INT_FAST16_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INT_FAST16_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__INT_FAST32_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffL")))))))
   (FUNCALL #1# "__INT_FAST32_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INT_FAST32_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__INT_FAST64_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffL")))))))
   (FUNCALL #1# "__INT_FAST64_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INT_FAST64_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__INT_FAST8_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7f")))))))
   (FUNCALL #1# "__INT_FAST8_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "signed"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "char")))))))
   (FUNCALL #1# "__INT_FAST8_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__INT_LEAST16_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fff")))))))
   (FUNCALL #1# "__INT_LEAST16_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "short"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INT_LEAST16_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16")))))))
   (FUNCALL #1# "__INT_LEAST32_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffff")))))))
   (FUNCALL #1# "__INT_LEAST32_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INT_LEAST32_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "32")))))))
   (FUNCALL #1# "__INT_LEAST64_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffL")))))))
   (FUNCALL #1# "__INT_LEAST64_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__INT_LEAST64_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__INT_LEAST8_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7f")))))))
   (FUNCALL #1# "__INT_LEAST8_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "signed"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "char")))))))
   (FUNCALL #1# "__INT_LEAST8_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__INT_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffff")))))))
   (FUNCALL #1# "__INT_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "32")))))))
   (FUNCALL #1# "__LDBL_DECIMAL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "21")))))))
   (FUNCALL #1# "__LDBL_DENORM_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "3.64519953188247460252840593361941982e-4951L")))))))
   (FUNCALL #1# "__LDBL_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "18")))))))
   (FUNCALL #1# "__LDBL_EPSILON__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.08420217248550443400745280086994171e-19L")))))))
   (FUNCALL #1# "__LDBL_HAS_DENORM__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__LDBL_HAS_INFINITY__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__LDBL_HAS_QUIET_NAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__LDBL_MANT_DIG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__LDBL_MAX_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4932")))))))
   (FUNCALL #1# "__LDBL_MAX_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16384")))))))
   (FUNCALL #1# "__LDBL_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1.18973149535723176502126385303097021e"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :+))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4932L")))))))
   (FUNCALL #1# "__LDBL_MIN_10_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4931"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__LDBL_MIN_EXP__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16381"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__LDBL_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "3.36210314311209350626267781732175260e-4932L")))))))
   (FUNCALL #1# "__LONG_LONG_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffLL")))))))
   (FUNCALL #1# "__LONG_LONG_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__LONG_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffL")))))))
   (FUNCALL #1# "__LONG_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__LP64__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__MMX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__NO_INLINE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__ORDER_BIG_ENDIAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4321")))))))
   (FUNCALL #1# "__ORDER_LITTLE_ENDIAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1234")))))))
   (FUNCALL #1# "__ORDER_PDP_ENDIAN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "3412")))))))
   (FUNCALL #1# "__PIC__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__PIE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__PRAGMA_REDEFINE_EXTNAME"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__PTRDIFF_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffffffffffffL")))))))
   (FUNCALL #1# "__PTRDIFF_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__PTRDIFF_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__REGISTER_PREFIX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE 'LANGUAGE.C.PREPROCESSOR.EVALUATOR::EMPTY-MACRO)))
   (FUNCALL #1# "__SCHAR_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7f")))))))
   (FUNCALL #1# "__SCHAR_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__SEG_FS"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__SEG_GS"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__SHRT_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fff")))))))
   (FUNCALL #1# "__SHRT_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16")))))))
   (FUNCALL #1# "__SIG_ATOMIC_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffff")))))))
   (FUNCALL #1# "__SIG_ATOMIC_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "__SIG_ATOMIC_MAX__"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE "1"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__SIG_ATOMIC_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__SIG_ATOMIC_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "32")))))))
   (FUNCALL #1# "__SIZEOF_DOUBLE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__SIZEOF_FLOAT128__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16")))))))
   (FUNCALL #1# "__SIZEOF_FLOAT80__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16")))))))
   (FUNCALL #1# "__SIZEOF_FLOAT__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4")))))))
   (FUNCALL #1# "__SIZEOF_INT128__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16")))))))
   (FUNCALL #1# "__SIZEOF_INT__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4")))))))
   (FUNCALL #1# "__SIZEOF_LONG_DOUBLE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "16")))))))
   (FUNCALL #1# "__SIZEOF_LONG_LONG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__SIZEOF_LONG__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__SIZEOF_POINTER__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__SIZEOF_PTRDIFF_T__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__SIZEOF_SHORT__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__SIZEOF_SIZE_T__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "8")))))))
   (FUNCALL #1# "__SIZEOF_WCHAR_T__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4")))))))
   (FUNCALL #1# "__SIZEOF_WINT_T__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "4")))))))
   (FUNCALL #1# "__SIZE_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffffffffffUL")))))))
   (FUNCALL #1# "__SIZE_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__SIZE_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "64")))))))
   (FUNCALL #1# "__SSE2_MATH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__SSE2__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__SSE_MATH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__SSE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__STDC_HOSTED__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__STDC_IEC_559_COMPLEX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__STDC_IEC_559__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__STDC_ISO_10646__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "201706L")))))))
   (FUNCALL #1# "__STDC_UTF_16__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__STDC_UTF_32__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__STDC_VERSION__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "201710L")))))))
   (FUNCALL #1# "__STDC__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__UINT16_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "c")))))))
   (FUNCALL #1# "__UINT16_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffff")))))))
   (FUNCALL #1# "__UINT16_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "short"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT32_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :CONCATENATION)
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "c")))
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "U")))))))))
   (FUNCALL #1# "__UINT32_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffU")))))))
   (FUNCALL #1# "__UINT32_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT64_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :CONCATENATION)
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "c")))
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "UL")))))))))
   (FUNCALL #1# "__UINT64_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffffffffffUL")))))))
   (FUNCALL #1# "__UINT64_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT8_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "c")))))))
   (FUNCALL #1# "__UINT8_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xff")))))))
   (FUNCALL #1# "__UINT8_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "char")))))))
   (FUNCALL #1# "__UINTMAX_C"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "c") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :CONCATENATION)
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "c")))
                   (1 :OPERAND
                    (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                         "UL")))))))))
   (FUNCALL #1# "__UINTMAX_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffffffffffUL")))))))
   (FUNCALL #1# "__UINTMAX_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINTPTR_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffffffffffUL")))))))
   (FUNCALL #1# "__UINTPTR_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT_FAST16_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffffffffffUL")))))))
   (FUNCALL #1# "__UINT_FAST16_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT_FAST32_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffffffffffUL")))))))
   (FUNCALL #1# "__UINT_FAST32_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT_FAST64_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffffffffffUL")))))))
   (FUNCALL #1# "__UINT_FAST64_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT_FAST8_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xff")))))))
   (FUNCALL #1# "__UINT_FAST8_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "char")))))))
   (FUNCALL #1# "__UINT_LEAST16_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffff")))))))
   (FUNCALL #1# "__UINT_LEAST16_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "short"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT_LEAST32_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffU")))))))
   (FUNCALL #1# "__UINT_LEAST32_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT_LEAST64_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffffffffffUL")))))))
   (FUNCALL #1# "__UINT_LEAST64_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "long"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__UINT_LEAST8_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xff")))))))
   (FUNCALL #1# "__UINT_LEAST8_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "char")))))))
   (FUNCALL #1# "__USER_LABEL_PREFIX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE 'LANGUAGE.C.PREPROCESSOR.EVALUATOR::EMPTY-MACRO)))
   (FUNCALL #1# "__VERSION__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :HEADER-NAME :KIND
                                                      :LOCAL :NAME
                                                      "9.2.1 20200306")))))))
   (FUNCALL #1# "__WCHAR_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0x7fffffff")))))))
   (FUNCALL #1# "__WCHAR_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "__WCHAR_MAX__"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :-))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE "1"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__WCHAR_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__WCHAR_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "32")))))))
   (FUNCALL #1# "__WINT_MAX__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0xffffffffU")))))))
   (FUNCALL #1# "__WINT_MIN__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "0U")))))))
   (FUNCALL #1# "__WINT_TYPE__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "unsigned"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "int")))))))
   (FUNCALL #1# "__WINT_WIDTH__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "32")))))))
   (FUNCALL #1# "__amd64"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__amd64__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__code_model_small__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__gnu_linux__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__has_include"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "STR") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "__has_include__"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "STR"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__has_include_next"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::FUNCTION-LIKE-MACRO
              :PARAMETERS (LIST "STR") :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "__has_include_next__"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|(|))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :IDENTIFIER :NAME
                                                      "STR"))
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :PUNCTUATOR :WHICH
                                                      :|)|)))))))
   (FUNCALL #1# "__k8"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__k8__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__linux"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__linux__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__pic__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__pie__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "2")))))))
   (FUNCALL #1# "__unix"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__unix__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__x86_64"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "__x86_64__"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "linux"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1")))))))
   (FUNCALL #1# "unix"
            (LOAD-TIME-VALUE
             (MAKE-INSTANCE
              'LANGUAGE.C.PREPROCESSOR.EVALUATOR::OBJECT-LIKE-MACRO
              :REPLACEMENT
              (LET ((#2# . #3#))
                (LIST
                 (ARCHITECTURE.BUILDER-PROTOCOL:NODE (#2# :NUMBER :VALUE
                                                      "1"))))))))) 
