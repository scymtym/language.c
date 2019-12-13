;;;; grammar.lisp --- Grammar for C18 (excluding the preprocessor language)
;;;;
;;;; Copyright (C) 2019 Daniel Kochmański
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Daniel Kochmański
;;;;         Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; https://web.archive.org/web/20181230041359if_/http://www.open-std.org/jtc1/sc22/wg14/www/abq/c17_updated_proposed_fdis.pdf

(cl:in-package #:language.c.c.parser)


;;; A.1.1 Lexical elements

(defrule token
    (or keyword identifier constant string-literal punctuator))

;;; A.1.2 Keywords

(deftokens (keyword t :skippable whitespace*)
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

;;; A.2.1 Expression

(defrule primary-expression
    (or identifier
        constant
        string-literal
        expression/parentheses
        generic-selection))

(defrule primary-expression/?s
    (and primary-expression whitespace*)
  (:function first))

(defrule generic-selection
    (and keyword-|_Generic| punctuator-|(| assignment-expression punctuator-|,| generic-assoc-list punctuator-|)|))

(defrule generic-assoc-list
    (+ generic-association))

(defrule generic-association
    (and (or |default| type-name) punctuator-|:| assignment-expression))

;; TODO much of the postfix stuff is duplicated in the shared grammar
(defrule postfix-expression
    (or postfix-expression/primary
        (and type-name/parenthses
             punctuator-{ initializer-list (* punctuator-|,|) punctuator-}
             postfix-expression*)))

(defrule postfix-expression/primary
    (and primary-expression/?s postfix-expression*)
  (:destructure (primary postfix)
    (if postfix
        (reduce #'funcall (reverse postfix) :initial-value primary :from-end t) ; TODO avoid reverse
        primary)))

(defrule postfix-expression*
    ;; TODO this was (* ...) in the original grammar
    (* (or subscript
           arguments
           (and punctuator-|.| identifier)
           (and punctuator--> identifier)
           postfix-increment
           (and punctuator--))))

(defrule subscript
    (and punctuator-[ expression punctuator-])
  (:function second)
  (:lambda (index &bounds start end)
    (lambda (expression)
      (bp:node* (:subscript :bounds (cons start end))
        (1 :expression expression)
        (1 :index      index)))))

(defrule arguments
    (and punctuator-|(| (* argument-expression-list) punctuator-|)|)
  (:function second))

(defrule argument-expression-list
    (and assignment-expression (* (and punctuator-|,| argument-expression))))

(defrule postfix-increment
    punctuator-++
  (:lambda (operator &bounds start end)
    (declare (ignore operator))
    (lambda (expression)
      (bp:node* (:postfix-increment :bounds (cons start end))
        (1 :expression expression)))))

(defrule unary-expression
    (or postfix-expression
        (and punctuator-++ unary-expression)
        (and punctuator-- unary-expression)
        (and unary-operators cast-expression)
        (and keyword-|sizeof| unary-expression)
        (and keyword-|sizeof| type-name/parenthses)
        (and keyword-|_Alignof| type-name/parenthses)))

(defrule unary-operators
    (or punctuator-& punctuator-* punctuator-+ punctuator-- punctuator-~ punctuator-!))

(defrule cast-expression
    (or unary-expression
        (and type-name/parentheses cast-expression)))

(defrule assignment-expression
    ;; TODO UNARY-EXPRESSION is not here in the original grammar
    (or proper-assignment-expression conditional-expression unary-expression ; TODO CONDITIONAL-EXPRESSION is in shared grammar => no postfix-expression
        ))

(defrule proper-assignment-expression
    (and unary-expression assignment-operator assignment-expression)
  (:destructure (left operator right &bounds start end)
    (bp:node* (:assignment-expression :operator operator
                                      :bounds   (cons start end))
      (1 :left  left)
      (1 :right right))))

(deftokens (assignment-operator t :skippable whitespace*)
  = *= /= %= += -= <<= >>= &= ^= \|=)

(defrule expression
    (and assignment-expression (* expression-rest))
  (:destructure (first second)
    (if second
        (list* first second)
        first)))

(defrule expression-rest
    (and punctuator-|,| assignment-expression)
  (:function second))

(define-bracket-rule expression/parentheses (punctuator-|(| punctuator-|)|)
    expression)


;;; A.2.2 Declarations

(defrule declaration
    (or declaration//
        static_assert-declaration))

(defrule declaration//
    (and declaration-specifiers (? init-declarator-list) punctuator-|;|)
  (:destructure (specifiers inits semicolon &bounds start end)
    (declare (ignore semicolon))
    (bp:node* (:declaration :bounds (cons start end))
      (* :specifier specifiers)
      (* :init      inits))))

(defrule declaration-specifiers
    (+ (or storage-class-specifier
           type-specifier
           type-qualifier
           function-specifier
           alignment-specifier)))

(define-separator-list-rule init-declarator-list
  init-declarator punctuator-|,|)

#+old (defrule init-declarator-list
    (and init-declarator (* init-declarator-list*)))

#+old (defrule init-declarator-list*
    (and |,| init-declarator)
  (:function second))

(defrule init-declarator
    (and declarator (? (and punctuator-|=| initializer)))
  (:destructure (name (&optional equals initializer) &bounds start end)
    (declare (ignore equals))
    (bp:node* (:init-declarator :bounds (cons start end))
      (1    :name        name)
      (bp:? :initializer initializer))))

(defrule storage-class-specifier
    (or keyword-|typedef|
        keyword-|extern|
        keyword-|static|
        keyword-|_Thread_local|
        keyword-|auto|
        keyword-|register|))

(defrule type-specifier
    (or keyword-|void|
        keyword-|char|
        keyword-|short|
        keyword-|int|
        keyword-|long|
        keyword-|float|
        keyword-|double|
        keyword-|signed|
        keyword-|unsigned|
        keyword-|_Bool|
        keyword-|_Complex|
        atomic-type-specifier
        struct-or-union-specifier
        enum-specifier
        ; typedef-name TODO uncomment and figure out how to make function-definition work despite it
        ))

(defrule struct-or-union-specifier
    (or (and struct-or-union (? identifier) punctuator-{ struct-declaration-list punctuator-})
        (and struct-or-union identifier))
  (:destructure (keyword name &optional open body close &bounds start end)
    (declare (ignore open close))
    (bp:node* (keyword :bounds (cons start end))
      (1 :name name)
      (* :body body))))

(defrule struct-or-union
    (or keyword-|struct| keyword-|union|))

(defrule struct-declaration-list
    (+ struct-declaration))

(defrule struct-declaration
    (or struct-declaration/declarator
        static_assert-declaration))

(defrule struct-declaration/declarator
    (and (+ specifier-qualifier) (* struct-declarator) punctuator-|;|)
  (:destructure (qualifiers declarators semicolon &bounds start end)
    (declare (ignore semicolon))
    (bp:node* (:struct-declarator :bounds (cons start end))
      (* :qualifier  qualifiers)
      (* :declarator declarators))))

(defrule specifier-qualifier
    (or type-specifier
        type-qualifier
        alignment-specifier)
  (:lambda (specifier &bounds start end)
    (bp:node* (:specifier-qualifier :specifier specifier
                                    :bounds    (cons start end)))))
(defrule struct-declarator
    (or struct-declarator/proper
        declarator))

(defrule struct-declarator/proper
    (and (? declarator) punctuator-|:| constant-expression)
  (:destructure (declarator colon expression &bounds start end)
    (declare (ignore colon))
    (bp:node* (:struct-declarator :bounds (cons start end))
      (bp:? :declarator declarator)
      (1    :expression expression))))

(defrule enum-specifier
    (and keyword-|enum| (or (and (? identifier) enum-body)
                            (and identifier)))
  (:function second)
  (:destructure (name &optional enumerators &bounds start end)
    (bp:node* (:enum :bounds (cons start end))
      (bp:? :name       name)
      (*    :enumerator enumerators))))

(defrule enum-body
    (and punctuator-{ enumerator-list (? punctuator-|,|) punctuator-})
  (:function second))

(define-separator-list-rule enumerator-list
    enumerator punctuator-|,|)

(defrule enumerator
    (and identifier #|TODO enumeration-constant|# (? (and punctuator-|=| constant-expression)))
  (:destructure (name (&optional equals value) &bounds start end)
    (declare (ignore equals))
    (bp:node* (:enumerator :bounds (cons start end))
      (1    :name  name)
      (bp:? :value value))))

(defrule atomic-type-specifier
    (and keyword-|_Atomic| punctuator-|(| type-name punctuator-|)|))

(defrule type-qualifier
    (or keyword-|const| keyword-|restrict| keyword-|volatile| keyword-|_Atomic|))

(defrule function-specifier
    (or keyword-|inline| keyword-|_Noreturn|))

(defrule alignment-specifier
    (and keyword-|_Alignas| punctuator-|(| (or type-name constant-expression) punctuator-|)|))

(defrule declarator
    (and (? pointer-list) direct-declarator)
  (:destructure (pointers declarator &bounds start end)
    (if pointers
        (bp:node* (:declarator :pointer? pointers :bounds (cons start end))
          (1 :declarator declarator))
        declarator)))

(defrule declarator/parentheses
    (and punctuator-|(| declarator punctuator-|)|)
  (:function second))

(defrule direct-declarator
    (and (or identifier declarator/parentheses) direct-declarator*)
  (:destructure (name suffixes &bounds start end)
    (bp:node* (:direct-declarator :bounds (cons start end))
      (1 :name   name)
      (* :suffix suffixes))))

(defrule direct-declarator*
    (* (or (and punctuator-[ (? type-qualifier-list) (? assignment-expression) punctuator-])
           (and punctuator-[ keyword-|static| (? type-qualifier-list) assignment-expression punctuator-])
           (and punctuator-[ type-qualifier-list keyword-|static| assignment-expression punctuator-])
           (and punctuator-[ (? type-qualifier-list) punctuator-* punctuator-])
           parameter-type-list
           identifier-list/parentheses)))

(defrule identifier-list/parentheses
    (and punctuator-|(| (? identifier-list) punctuator-|)|)
  (:function second))

(defrule pointer-list
    (+ pointer))

(defrule pointer
    (and punctuator-* (? type-qualifier-list))
  (:function second))

(defrule type-qualifier-list ; TODO remove this rule?
    (+ type-qualifier))

(define-bracket-rule parameter-type-list (punctuator-|(| punctuator-|)|)
    (or (and parameter-list punctuator-|,| punctuator-|...|)
        parameter-list))

(define-separator-list-rule parameter-list
  parameter-declaration punctuator-|,|)

(defrule parameter-declaration
    (and declaration-specifiers (or declarator
                                    (? abstract-declarator)))
  (:destructure (specifiers declarator &bounds start end)
    (bp:node* (:parameter-declaration :bounds (cons start end))
      (* :specifier  specifiers)
      (1 :declarator declarator))))

;;; `identifier-list' defined in shared grammar

(defrule type-name
    (and specifier-qualifier-list (? abstract-declarator)))

(define-bracket-rule type-name/parenthses (punctuator-|(| punctuator-|)|)
    type-name)

(defrule abstract-declarator
    (or pointer-list
        (and (? pointer-list) direct-abstract-declarator)))

(defrule direct-abstract-declarator
    (or (and punctuator-|(| abstract-declarator punctuator-|)|)
        (and (? direct-abstract-declarator)
             (or (and punctuator-[ (? type-qualifier-list) (? assignment-expression) punctuator-])
                 (and punctuator-[ keyword-|static| (? type-qualifier-list) assignment-expression punctuator-])
                 (and punctuator-[ type-qualifier-list keyword-|static| assignment-expression punctuator-])
                 (and punctuator-[ punctuator-* punctuator-])
                 (and punctuator-|(| (? parameter-type-list) punctuator-|)|)))))

(defrule typedef-name
    identifier)

(defrule initializer
    (or assignment-expression
        brace-initializer))

(define-bracket-rule brace-initializer (punctuator-{ punctuator-})
    (and initializer-list (? punctuator-|,|))
  (:destructure (initializers comma &bounds start end)
    (bp:node* (:brace-initializer :comma  comma
                                  :bounds (cons start end))
      (* :initializer initializers))))

(define-separator-list-rule initializer-list
  initializer-list-entry punctuator-|,|)

(defrule initializer-list-entry
    (and (? designation) initializer)
  (:destructure (designation initializer &bounds start end)
    (bp:node* (:initializer :bounds (cons start end))
      (* :designation designation)
      (1 :expression  initializer))))

(defrule designation
    (and (+ designator) punctuator-=)
  (:function first))

(defrule designator
    (or array-index-designator
        member-designator))

(define-bracket-rule array-index-designator (punctuator-[ punctuator-])
    constant-expression
  (:lambda (index &bounds start end)
    (bp:node* (:array-index-designator :bounds (cons start end))
      (1 :index index))))

(defrule member-designator
    (and punctuator-|.| identifier)
  (:function second)
  (:lambda (member &bounds start end)
    (bp:node* (:member-designator :bounds (cons start end))
      (1 :member member))))

(defrule static_assert-declaration
    (and keyword-|_Static_assert| punctuator-|(| constant-expression punctuator-|,| string-listeral punctuator-|)| punctuator-|;|))


;;; A.2.3 Statements

(defrule statement
    (or labeled-statement
        compound-statement
        selection-statement
        iteration-statement
        jump-statement
        expression-statement))

(defrule labeled-statement
    (and (or (and keyword-|case| constant-expression)
             (and keyword-|default|)
             (and identifier))
         punctuator-|:| statement))

(define-bracket-rule compound-statement (punctuator-{ punctuator-})
    (* block-item/?s))

(defrule block-item
    (or declaration
        statement))

(defrule block-item/?s
    (and block-item whitespace*)
  (:function first))

(defrule expression-statement
    (and (? expression) punctuator-|;|)
  (:function first))

(defrule selection-statement
    (or if-statement switch-statement))

(defrule if-statement
    (and keyword-|if| expression/parentheses statement (? (and keyword-|else| statement)))
  (:destructure (keyword1 test then &optional keyword2 else
                 &bounds start end)
    (declare (ignore keyword1 keyword2))
    (bp:node* (:if-statement :bounds (cons start end))
      (1    :test test)
      (1    :then then)
      (bp:? :else else))))

(defrule switch-statement
    (and keyword-|switch| expression/parentheses statement)
  (:function rest)
  (:destructure (value body &bounds start end)
    (bp:node* (:switch-statement :bounds (cons start end))
      (1 :value value)
      (1 :body  body))))

(defrule iteration-statement
    (or while-statement
        do-statement
        for-statement))

(defrule while-statement
    (and keyword-|while| expression/parentheses statement)
  (:function rest)
  (:destructure (test body &bounds start end)
    (bp:node* (:while-statement :bounds (cons start end))
      (1 :test test)
      (1 :body body))))

(defrule do-statement
    (and keyword-|do| statement keyword-|while| expression/parentheses punctuator-|;|)
  (:destructure (keyword1 body keyword2 test semicolon &bounds start end)
    (declare (ignore keyword1 keyword2 semicolon))
    (bp:node* (:do-statement :bounds (cons start end))
      (1 :test test)
      (1 :body body))))

(defrule for-statement
    (and keyword-|for| punctuator-|(|
         (or (and declaration)
             (and (? expression) punctuator-|;|))
         (? expression) punctuator-|;|
         (? expression)
         punctuator-|)| statement)
  (:destructure (keyword open (init &optional semi1) test semi2 step close body
                 &bounds start end)
    (declare (ignore keyword open semi1 semi2 close))
    (bp:node* (:for-statement :bounds (cons start end))
      (bp:? :init init)
      (bp:? :test test)
      (bp:? :step step)
      (1    :body body))))

(defrule jump-statement
    (and (or goto-statement
             continue-statement
             break-statement
             return-statement)
         punctuator-|;|)
  (:function first))

(defrule goto-statement
    (and keyword-|goto| identifier)
  (:function second)
  (:lambda (label &bounds start end)
    (bp:node* (:goto-statement :bounds (cons start end))
      (1 :label label))))

(defrule continue-statement
    keyword-|continue|
  (:lambda (keyword &bounds start end)
    (declare (ignore keyword))
    (bp:node* (:continue-statement :bounds (cons start end)))))

(defrule break-statement
    keyword-|break|
  (:lambda (keyword &bounds start end)
    (declare (ignore keyword))
    (bp:node* (:break-statement :bounds (cons start end)))))

(defrule return-statement
    (and keyword-|return| (? expression))
  (:function second)
  (:lambda (value &bounds start end)
    (bp:node* (:return-statement :bounds (cons start end))
      (bp:? :value value))))


;;; A.2.4 External definitions

(defrule translation-unit/whitespace
    (and whitespace* translation-unit whitespace*)
  (:function second))

(defrule translation-unit
    (+ external-declaration)
  (:lambda (declarations &bounds start end)
    (bp:node* (:translation-unit :bounds (cons start end))
      (* :declaration declarations))))

(defrule translation-unit*
    (or (and external-declaration translation-unit*) ; TODO
        (& (? character))))

(defrule external-declaration
    (or function-definition
        declaration))

(defrule function-definition
    (and declaration-specifiers declarator (* declaration) compound-statement)
  (:destructure (specifiers (pointer (name parameters)) declarations body
                 &bounds start end)
    (bp:node* (:function-definition :bounds (cons start end))
      (1 :name        name)
      (* :return      specifiers)
      (* :parameter   parameters)
      (* :declaration declarations)
      (* :body        body))))
