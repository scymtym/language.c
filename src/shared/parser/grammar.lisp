;;;; grammar.lisp --- Grammar for things shared between C18 and the preprocessor language
;;;;
;;;; Copyright (C) 2019 Daniel Kochmański
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Daniel Kochmański
;;;;         Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.shared.parser)


;;; A.1 Lexical grammar

(defrule whitespace/same-line
    (or (character-ranges #\Space #\Tab)
        (and #\\ #\Newline) ; "5.1.1.2 Translation phases" Phase 2
        parser.common-rules:c-style-comment/delimited/trimmed)
  (:error-report nil)
  (:use-cache nil))

(defrule whitespace/same-line+
    (+ whitespace/same-line)
  (:error-report nil)
  (:use-cache nil))

(defrule whitespace/same-line*
    (* whitespace/same-line)
  (:error-report nil)
  (:use-cache nil))

(defrule whitespace
    (or whitespace/same-line
        (character-ranges #\Newline
                          #.(code-char 11)  ; vertical tab
                          #.(code-char 12)) ; form feed
        parser.common-rules:c-style-comment/rest-of-line/trimmed)
  (:error-report nil)
  (:use-cache nil))

(defrule comment
    (or parser.common-rules:c-style-comment/delimited/trimmed
        parser.common-rules:c-style-comment/rest-of-line/trimmed)
  (:error-report nil))

;;; A.1.2 Keywords

#+unused? (deftokens (keyword t :requires-separation? t)
  |if| |else|) ; used by cpp and c

#+unused (deftokens (keyword t)
  |auto|          |extern|        |short|         |while|
  |break|         |float|         |signed|        |_Alignas|
  |case|          |for|           |sizeof|        |_Alignof|
  |char|          |goto|          |static|        |_Atomic|
  |const|         |if|            |struct|        |_Bool|
  |continue|      |inline|        |switch|        |_Complex|
  |default|       |int|           |typedef|       |_Generic|
  |double|        |long|          |union|         |_Imaginary|
  |do|            |register|      |unsigned|      |_Noreturn|
  |else|          |restrict|      |void|          |_Static_assert|
  |enum|          |return|        |volatile|      |_Thread_local|)

;;; A.1.3 Identifiers

;;; Defined in A.2.2 but shared between preprocessor and proper C.
(define-separator-list-rule identifier-list identifier punctuator-|,|)

(defrule identifier                     ; TODO exclude keywords?
    (and identifier-nondigit (* (or identifier-nondigit digit)))
  (:text t)
  (:lambda (name &bounds start end)
    (bp:node* (:identifier :name name :bounds (cons start end)))))

(defrule identifier-nondigit
    (or nondigit universal-character-name))

(defrule nondigit
    (character-ranges (#\a #\z) (#\A #\Z) #\_))

(defrule digit ; TODO where is this used? why define decimal-digit in A.1.5?
    (digit-char-p character))

;;; A.1.4 Universal character names
(defrule universal-character-name
    (or (and #\\ #\u hex-quad)
        (and #\\ #\U hex-quad hex-quad)))

(defrule hex-quad
    (and hexadecimal-digit
         hexadecimal-digit
         hexadecimal-digit
         hexadecimal-digit))

;;; A.1.5 Constants

(defrule constant
    (or floating-constant
        integer-constant
        enumeration-constant
        character-constant))

(defrule integer-constant
    (and (or parser.common-rules:integer-literal/hexadecimal/prefix  ; TODO is the prefix correct?
             parser.common-rules:integer-literal/octal/prefix ; TODO is the prefix correct?
             parser.common-rules:integer-literal/decimal)
         ; (or decimal-constant octal-constant hexadecimal-constant)
         (? integer-suffix))
  (:destructure (value (&optional size unsigned?) &bounds start end)
    (bp:node* (:constant :type      :integer
                         :value     value
                         :size      size
                         :unsigned? unsigned?
                         :bounds    (cons start end)))))

#+no (defrule decimal-constant
    (and nonzero-digit (* digit)))

#+no (defrule octal-constant
    (and #\0 (* octal-digit)))

#+no (defrule hexadecimal-constant
    (and hexadecimal-prefix (+ hexadecimal-digit)))

#+no (defrule hexadecimal-prefix
    (or "0x" "0X"))

#+no (defrule nonzero-digit
    (character-ranges (#\1 #\9)))

(defrule octal-digit
    (character-ranges (#\0 #\7)))

#+no (defrule hexadecimal-digit
    (or digit
        (character-ranges (#\a #\f) (#\A #\F))))

(defrule integer-suffix
    (or (and unsigned-suffix (? size-suffix))
        (and size-suffix     (? unsigned-suffix)))
  (:destructure (first second)
    (if (eq first :unsigned)
        (list second first)
        (list first second))))

(defrule size-suffix
    (or long-long-suffix long-suffix))

(defrule unsigned-suffix
    (or #\u #\U)
  (:constant :unsigned))

(defrule long-suffix
    (or #\l #\L)
  (:constant :long))

(defrule long-long-suffix
    (or "ll" "LL")
  (:constant :long-long))

(defvar *floating-point-constants?* nil)

(defrule floating-constant
    (or decimal-floating-constant
        hexadecimal-floating-constant)
  (:when *floating-point-constants?*))

(defrule decimal-floating-constant
    (and (or (and fractional-constant (? exponent-part))
             (and digit-sequence      exponent-part))
         (? floating-suffix))
  (:destructure ((mantissa exponent) suffix &bounds start end)
    (let ((value (if exponent
                     (* mantissa (expt 10 exponent))
                     mantissa)))
      (bp:node* (:constant :type   :floating
                           :size   suffix
                           :value  value
                           :bounds (cons start end))))))

(defrule fractional-constant
    (or (and (? parser.common-rules::integer-digits/decimal) parser.common-rules::float-decimals)
        (and parser.common-rules::integer-digits/decimal     #\.))
  (:destructure (digits decimals)
    (+ (parse-integer digits) (if (realp decimals)
                                  decimals
                                  0))))

(defrule exponent-part
    (and (~ #\e) (? sign) parser.common-rules::integer-digits/decimal)
  (:function rest)
  (:destructure (sign exponent)
    (* (or sign 1) (parse-integer exponent))))

(defrule sign
    (or sign-+ sign--))

(defrule sign-+
    #\+
  (:constant +1))

(defrule sign--
    #\-
  (:constant -1))

(defrule digit-sequence
    (+ digit))

(defrule hexadecimal-floating-constant
    (and "0x" ; hexadecimal-prefix
         (or hexadecimal-fractional-constant
             hexadecimal-digit-sequence)
         binary-exponent-part
         (? floating-suffix))
  (:lambda (stuff &bounds start end)
    (bp:node* (:constant :value stuff :bounds (cons start end)))))

(defrule hexadecimal-fractional-constant
    (or (and (? hexadecimal-digit-sequence) #\. hexadecimal-digit-sequence)
        (and hexadecimal-digit-sequence     #\.)))

(defrule binary-exponent-part
    (and (~ #\p) (? sign) digit-sequence))

(defrule hexadecimal-digit-sequence
    (+ (character-ranges (#\0 #\9) (#\a #\f)) ; hexadecimal-digit
       ))

(defrule floating-suffix
    (or floating-suffix/short floating-suffix/long))

(defrule floating-suffix/short
    (character-ranges #\f #\F)
  (:constant :short))

(defrule floating-suffix/long
    (character-ranges #\f #\F)
  (:constant :long))

(defrule enumeration-constant
    identifier)

(defrule character-constant
    (and (? (or #\L (~ #\u))) #\' c-char-sequence #\')
  (:function third)
  (:lambda (value &bounds start end)
    (bp:node* (:constant :value value :bounds (cons start end)))))

(defrule c-char-sequence
    (+ c-char)
  (:text t))

(defrule c-char
    (or (not (or #\' #\\ #\Newline))
        escape-sequence))

(defrule escape-sequence
    (or simple-escape-sequence
        octal-escape-sequence
        hexadecimal-escape-sequence
        universal-character-name))

(defrule simple-escape-sequence
    (or "\\'" "\\\"" "\\?" "\\\\"
        "\\a" "\\b" "\\f" "\\n" "\\r" "\\t" "\\v"))

(defrule octal-escape-sequence
    (and "\\" octal-digit (? (and octal-digit (? octal-digit)))))

(defrule hexadecimal-escape-sequence
    (and "\\x" (+ hexadecimal-digit)))

;;; A.1.6 String literals

(defrule string-literal
    (and (? encoding-prefix) #\" s-char-sequence #\")
  (:destructure (encoding open value close &bounds start end)
    (declare (ignore open close))
    (bp:node* (:string-literal :value    value
                               :encoding encoding
                               :bounds   (cons start end)))))

(defrule encoding-prefix
    (or "u8" "u" "U" "L"))

(defrule s-char-sequence
    (* s-char)
  (:text t))

(defrule s-char
    (or (not (or #\" #\\ #\newline))
        escape-sequence))


;;; A.1.7 Punctuators

(deftokens (punctuator t :whitespace whitespace/same-line*) ; TODO should this return a node? TODO CPP wants whitespace without newlines; C wants newlines
  ;; Two things are noteworthy here:
  ;; 1. Some of the tokens (such as "(", ";", ",") need escaping
  ;; 2. Order matters. An earlier entry must not be a prefix of a
  ;;    later entry. TODO Not true since `deftokens' currently
  ;;    generates disambiguations such as (and "&" (! "&")) but maybe
  ;;    we can avoid that.
  [ ] |(| |)| { } |.| ->
  / % << >> <= >= < > == != ^ && \|\|
  ++ -- & \| * + - ~ !
  ? |:| |;| |...|
  = *= /= %= += -= <<= >>= &= ^= \|=
  |,| |##| |#|
  |<:| |:>| <% %> |%:%:| |%:|)


;;; A.2


;;; A.2.1 Expression

(defrule primary-expression
    (or identifier
        constant
        string-literal
        expression/parentheses
        ; generic-selection
        ))

(defrule expression/parentheses
    (and punctuator-|(| conditional-expression punctuator-|)|)
  (:function second))

(defrule generic-selection
    (and |_Generic| punctuator-|(| assignment-expression punctuator-|,| generic-assoc-list punctuator-|)|))

(defrule generic-assoc-list
    (+ generic-association))

(defrule generic-association
    (or (and type-name punctuator-|:| assignment-expression)
        (and |default| punctuator-|:| assignment-expression)))

(defrule postfix-expression
    (or simple-postfix-expression
        (and punctuator-|(| type-name punctuator-|)|
             punctuator-{ initializer-list (* punctuator-|,|) punctuator-}
             postfix-expression-suffix)))

(defrule simple-postfix-expression
    (and primary-expression (and) #+no postfix-expression-suffix)
  (:destructure (primary suffix)
    (if suffix
        (break)
        primary)))

(defrule postfix-expression-suffix
    (* (or (and punctuator-[ expression punctuator-])
           (and punctuator-|(| (* argument-expression-list) punctuator-|)|)
           (and punctuator-|.| identifier)
           (and punctuator-|->| identifier)
           (and punctuator-|++|)
           (and punctuator-|-|))))

(defrule argument-expression-list
    (and assignment-expression (* (and punctuator-|,| argument-expression))))

#+later (defrule unary-expression
    (or (and unary-operator cast-expression)
             simple-postfix-expression)
    #+later (or postfix-expression
        (and |++| unary-expression)
        (and |-| unary-expression)
        (and unary-operators cast-expression)
        (and |sizeof| unary-expression)
        (and |sizeof| |(| type-name |)|)
        (and |_Alignof| |(| type-name |)|)))

(parser.common-rules.operators:define-unary-operator-rule unary-expression
  unary-operator simple-postfix-expression
  :skippable?-expression (and)
  :node-kind             :unary-expression)

(defrule unary-operator
    (or punctuator-& punctuator-* punctuator-+ punctuator--
        punctuator-~ punctuator-!))

(defrule cast-expression
    (or                      ; (and |(| type-name |)| cast-expression)
     unary-expression))

(parser.common-rules.operators:define-operator-rules
    (:skippable?-expression (and)
     :binary-node-kind      :binary-expression
     :ternary-node-kind     :ternary-expression)
  (3 conditional-expression    punctuator-? punctuator-|:|)
  (2 logical-or-expression     punctuator-\|\|)
  (2 logical-and-expression    punctuator-&&)
  (2 or-expression             punctuator-\|)
  (2 xor-expression            punctuator-^)
  (2 and-expression            punctuator-&)
  (2 equality-expression       (or punctuator-== punctuator-!=))
  (2 relational-expression     (or punctuator-<= punctuator->=
                                   punctuator-<  punctuator->))
  (2 shift-expression          (or punctuator-<< punctuator->>))
  (2 additive-expression       (or punctuator-+  punctuator--))
  (2 multiplicative-expression (or punctuator-*  punctuator-/ punctuator-%))
  cast-expression)

(defrule constant-expression
    conditional-expression
  (:use-cache nil))
