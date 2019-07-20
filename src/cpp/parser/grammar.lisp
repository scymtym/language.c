;;;; grammar.lisp --- Grammar for the C18 preprocessor language
;;;;
;;;; Copyright (C) 2019 Daniel Kochmański
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Daniel Kochmański
;;;;         Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; This grammar corresponds to a combination of "A.1 Lexical
;;;; grammar" and "A.3 Preprocessing directives".
;;;;
;;;; The lparen, replacement-list and pp-tokens rules and not defined
;;;; as rules.

(cl:in-package #:language.c.cpp.parser)

;;; A.1.1 Lexical elements

(deftokens (keyword-token t :whitespace           (* (or #\Space
                                                         #\Tab
                                                         parser.common-rules:c-style-comment/delimited
                                                         (and #\\ #\Newline)))
                            :requires-separation? t)
  |if| |ifdef| |ifndef|
  |else| |elif|
  |endif|

  |include|

  |define| |undef|

  |line| |error| |pragma|)

(defrule punctuator-token ; TODO can be avoided if shared grammar makes a node
    (and (! (and |#| keyword-token)) punctuator) ; TODO punctuactors eat newlines
  (:function second)
  (:lambda (which &bounds start end)
    (bp:node* (:punctuator :which which :bounds (cons start end)))))

(defrule word-token
    (and (! |#|) (+ (not (or whitespace new-line))))  ; TODO (not whitespace) seems suspicious
  (:function second)
  (:text t))

(defrule preprocessing-token
    (and (! |endif|)
         (or header-name
             identifier
             pp-number
             character-constant
             string-literal
             punctuator-token
             ; word-token
             ))
  (:function second))

;;; A.1.8 Header names

(defrule header-name
    (or (and h-header-name-start h-char-sequence #\>)
        (and q-header-name-start q-char-sequence #\"))
  (:destructure (kind name close &bounds start end)
    (declare (ignore close))
    (bp:node* (:header-name :kind kind :name name :bounds (cons start end)))))

(defrule h-header-name-start
    (and #\< (! #\<))
  (:constant :system))

(defrule h-char-sequence
    (+ (not (or #\newline #\>)))
  (:text t))

(defrule q-header-name-start
    #\"
  (:constant :local))

(defrule q-char-sequence
    (+ (not (or #\newline #\")))
  (:text t))


;;; A.1.9 Preprocessing numbers

#+no (defrule pp-number
    parser.common-rules:integer-literal
  (:lambda (value &bounds start end)
    (bp:node* (:number :value value :bounds (cons start end)))))

#+no (defrule pp-number
    (and (? |.|)
         parser.common-rules:integer-literal/decimal/no-sign
         pp-number-suffix)
  (:destructure (point digits suffix &bounds start end)
    (declare (ignore point suffix)) ; TODO how should this work?
    (bp:node* (:number :value digits :bounds (cons start end)))))

#+no (defrule pp-number-suffix
    (* (or (and (~ "e") sign)
           (and (~ "p") sign)
           identifier-nondigit
           ".")))

(defrule pp-number
    (and (? |.|)
         parser.common-rules::integer-digits/decimal
         (* (or parser.common-rules::integer-digits/decimal
                pp-number-suffix)))
  (:text t)
  (:lambda (value &bounds start end)
    (bp:node* (:number :value value :bounds (cons start end)))))

(defrule pp-number-suffix
    (or (and (~ #\e) #\- ; sign
             )
        (and (~ #\p) #\- ; sign TODO can we use sign?
             )
        identifier-nondigit
        "."))

;;; A.3

(defrule preprocessing-file
    (? group))

(defrule group
    (+ group-part)
  (:lambda (parts &bounds start end)
    (bp:node* (:group :bounds (cons start end))
      (* :part parts))))

(defrule group-part
    (or top-level-comment
        if-section
        control-line
        text-line
        (and |#| non-directive)))

(defrule top-level-comment
    (and (* (or #\Space #\Tab)) comment (* (or #\Space #\Tab)))
  (:constant nil))

;;; If

(defrule if-section
    (and if-group (? elif-groups) (? else-group) endif)
  (:destructure ((if-keyword if-test then-group) elif else-group keyword
                 &bounds start end)
    (declare (ignore elif keyword))
    (bp:node* (:if :kind if-keyword :bounds (cons start end))
      (*    :test if-test)
      (bp:? :then then-group)
      (bp:? :else else-group))))

(defrule if-group
    (and |#| (or (and (or |ifdef| |ifndef|) (and identifier)) ; TODO should this also be (+ pp-token)
                 (and |if|                  #|constant-expression|# (+ pp-token)))
         new-line (? (and (! (or elif-groups else-group)) group)))
  (:function rest)
  (:destructure ((keyword expression) newline (&optional guard group))
    (declare (ignore newline guard))
    (list keyword expression group)))

(defrule elif-groups
    (+ elif-group))

(defrule elif-group
    (and |#| |elif| (+ pp-token) #|constant-expression|#
         new-line (? (and (! (or elif-groups else-group)) group)) (? group))) ; TODO identical to if-group

(defrule else-group
    (and |#| |else| new-line (? group))
  (:function fourth))

(defrule endif
    (and |#| |endif| (? parser.common-rules:c-style-comment/rest-of-line) new-line)
  (:function second))

;;; Control lines

(defmacro define-control-line-rule
    (name keyword-expression expression (bindings &body body))
  `(defrule ,name
       (and ,keyword-expression ,expression)
     (:function second)
     (:destructure (,@bindings &bounds start end)
       ,@body)))

(defrule control-line
    (and (and) #+no (! |endif|)
         |#| (or include-line
                 define-identifier-line
                 undef-line
                 line-line
                 error-line
                 pragma-line
                 (and))
         (* (and (! new-line) whitespace))
         new-line)

  #+no (and |#| (or
                 (and define identifier replacement-list new-line)
                 (and define lparen (? identifier-list) |)| replacement-list new-line)
                 (and define lparen |...| |)| replacement-list new-line)
                 (and define lparen identifier-list |,| |...| |)| replacement-list new-line)
                 (and undef identifier new-line)
                 (and line pp-tokens new-line)
                 (and error (? pp-tokens) new-line)
                 (and pragma (? pp-tokens) new-line)
                 new-line))
  (:function third))

(define-control-line-rule include-line |include|
    (and (+ pp-token))
  ((filename)
   (bp:node* (:include :filename filename :bounds (cons start end)))))

(define-control-line-rule define-identifier-line |define| ; TODO lparen stuff not handled?
    (and identifier (? (and language.c.shared.parser::|(| identifier-list language.c.shared.parser::|)|))
         (* pp-token))
  ((name (&optional open parameters close) replacement)
   (declare (ignore open close))
   (if parameters
       (bp:node* (:function-like-macro :bounds (cons start end))
         (1 :name        name)
         (* :parameter   parameters)
         (* :replacement replacement))
       (bp:node* (:object-like-macro :bounds (cons start end))
         (1 :name        name)
         (* :replacement replacement)))))

(define-control-line-rule undef-line |undef|
    (and identifier)
  ((name)
   (bp:node* (:undef :name name :bounds (cons start end)))))

(define-control-line-rule line-line |line|
    (and (+ pp-token))
  ((tokens)
   (bp:node* (:line :tokens tokens :bounds (cons start end)))))

(define-control-line-rule error-line |error|
  (and (* pp-token))
  ((message)
   (bp:node* (:error :message message :bounds (cons start end)))))

(define-control-line-rule pragma-line |pragma|
    (and (* pp-token))
  ((tokens)
   (bp:node* (:pragma :tokens tokens :bounds (cons start end)))))

;;; Lexical stuff

(defrule text-line
    (or (and (+ pp-token) (! #\\) new-line)
        (and (and)        (* (or #\Space #\Tab comment)) (! #\\) #\Newline)) ; TODO space is temp
  (:function first)
  (:lambda (tokens &bounds start end)
    (bp:node* (:line :bounds (cons start end))
      (* :token tokens))))

(defrule non-directive
    (and (! keyword-token) (+ pp-token) new-line)
  (:function second))

(defrule pp-token
    (and (* (and (! new-line) whitespace)) ;  TODO define our own whitespace rule?
         preprocessing-token
         (* (and (! new-line) whitespace)))
  (:function second))

(defrule new-line
    (or #\newline <end-of-input>))

(defrule on-off-switch
    (or |ON| |OFF| |DEFAULT|))

(bp:with-builder ('list)
  (esrap:parse 'preprocessing-file
               "#define __ptr_t void *

1"))

#|
#ifdef __cplusplus
# define __END_DECLS foo
#else
# define __END_DECLS bar
#endif
|#


(bp:with-builder ('list)
  (esrap:parse 'preprocessing-file
               "# if (defined __cplusplus						\
      || (defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L))
#  define __inline	inline
# else
#  define __inline		/* No inline functions.  */
# endif
"))
