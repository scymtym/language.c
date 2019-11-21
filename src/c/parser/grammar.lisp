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
    (or keyword-token identifier constant string-literal punctator))

;;; A.1.2 Keywords

(deftokens (keyword-token t)
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
        |(expression)|
        generic-selection))

(defrule primary-expression/?ws
    (and primary-expression (* (or #\Space #\Tab #\Newline)))
  (:function first))

(defrule generic-selection
    (and |_Generic| |(| assignment-expression |,| generic-assoc-list |)|))

(defrule generic-assoc-list
    (+ generic-association))

(defrule generic-association
    (and (or |default| type-name) |:| assignment-expression))

;; TODO much of the postfix stuff is duplicated in the shared grammar
(defrule postfix-expression
    (or postfix-expression/primary
        (and |(type-name)|
             punctuator-{ initializer-list (* |,|) punctuator-}
             postfix-expression*)))

(defrule postfix-expression/primary
    (and primary-expression/?ws postfix-expression*)
  (:destructure (primary postfix)
    (reduce #'funcall (reverse postfix) :initial-value primary :from-end t))) ; TODO avoid reverse

(defrule postfix-expression*
    ;; TODO this was (* ...) in the original grammar
    (+ (or subscript
           arguments
           (and |.| identifier)
           (and |->| identifier)
           postfix-increment
           (and |-|))))

(defrule subscript
    (and punctuator-[ expression punctuator-])
  (:function second)
  (:lambda (index &bounds start end)
    (lambda (expression)
      (bp:node* (:subscript :bounds (cons start end))
        (1 :expression expression)
        (1 :index      index)))))

(defrule arguments
    (and punctator-|(| (* argument-expression-list) punctator-|)|))

(defrule argument-expression-list
    (and assignment-expression (* (and |,| argument-expression))))

(defrule postfix-increment
    |++|
  (:lambda (operator &bounds start end)
    (declare (ignore operator))
    (lambda (expression)
      (bp:node* (:postfix-increment :bounds (cons start end))
        (1 :expression expression)))))



(defrule unary-expression
    (or postfix-expression
        (and |++| unary-expression)
        (and |-| unary-expression)
        (and unary-operators cast-expression)
        (and |sizeof| unary-expression)
        (and |sizeof| |(type-name)|)
        (and |_Alignof| |(type-name)|)))

#+no (deftokens (unary-operators t) ; TODO these are still CL symbols. can't do this
    |&| |*| |+| |-| |~| |!|)

(defrule unary-operators
    (or |&| |*| |+| |-| |~| |!|))

(defrule cast-expression
    (or unary-expression
        (and |(| type-name |)| cast-expression)))

(defrule assignment-expression
    ;; TODO POSTFIX-EXPRESSION is not here in the original grammar
    (or proper-assignment-expression postfix-expression conditional-expression ; TODO CONDITIONAL-EXPRESSION is in shared grammar => no postfix-expression
        ))

(defrule proper-assignment-expression
    (and unary-expression assignment-operator assignment-expression)
  (:destructure (left operator right &bounds start end)
    (bp:node* (:assignment-expression :operator operator
                                      :bounds   (cons start end))
      (1 :left  left)
      (1 :right right))))

(deftokens (assignment-operator t)
  |=| |*=| |/=| |%=| |+=| |-=| |<<=| |>>=| |&=| |^=| |\|=|)

(define-separator-list-rule expression
  assignment-expression punctuator-|,|)

;; TODO unused?
(define-bracket-rule expression-parentheses (punctator-|(| punctator-|)|)
    expression)


;;; A.2.2 Declarations

(defrule declaration
    (or declaration//
        static_assert-declaration))

(defrule declaration//
    (and declaration-specifiers (? init-declarator-list) punctator-|;|)
  (:destructure (specifiers inits semicolon)
    (declare (ignore semicolon))
    (list :declaration specifiers inits)))

(defrule declaration-specifiers
    (* (or storage-class-specifier
           type-specifier
           type-qualifier
           function-specifier
           alignment-specifier)))

(define-separator-list-rule init-declarator-list
  init-declarator punctator-|,|)

#+old (defrule init-declarator-list
    (and init-declarator (* init-declarator-list*)))

#+old (defrule init-declarator-list*
    (and |,| init-declarator)
  (:function second))

(defrule init-declarator
    (or declarator
        (and declarator |=| initializer)))

(defrule storage-class-specifier
    (or |typedef|
        |extern|
        |static|
        |_Thread_local|
        |auto|
        |register|))

(defrule type-specifier
    (or |void|
        |char|
        |short|
        |int|
        |long|
        |float|
        |double|
        |signed|
        |unsigned|
        |_Bool|
        |_Complex|
        atomic-type-specifier
        struct-or-union-specifier
        enum-specifier
        ; typedef-name TODO uncomment and figure out how to make function-definition work despite it
        ))

(defrule struct-or-union-specifier
    (or (and struct-or-union (? identifier) { struct-declaration-list })
        (and struct-or-union identifier)))

(defrule struct-or-union
    (or |struct| |union|))

(defrule struct-declaration-list
    (+ struct-declaration))

(defrule struct-declaration
    (or (and specifier-qualifier-list (? struct-declarator-list) punctuator-|;|)
        static_assert-declaration))

(defrule specifier-qualifier-list
    (and (or type-specifier
             type-qualifier
             alignment-specifier)
         (? specifier-qualifier-list)))

(defrule struct-declarator-list
    (+ struct-declarator))

(defrule struct-declarator
 (or (and (? declarator) |:| constant-expression)
     declarator))

(defrule enum-specifier
    (and |enum| (or (and (? identifier) { enumerator-list (? |,|) })
                    (and identifier))))

(defrule enumerator-list
    (+ enumerator))

(defrule enumeratior ; TODO what?
    (or enumeration-constant
        (and enumeration-constant punctuator-|=| constant-expression)))

(defrule atomic-type-specifier
    (and |_Atomic| |(| type-name |)|))

(defrule type-qualifier
    (or |const| |restrict| |volatile| |_Atomic|))

(defrule function-specifier
    (or |inline| |_Noreturn|))

(defrule alignment-specifier
    (and |_Alignas| |(| (or type-name constant-expression) |)|))

(defrule declarator
    (and (? pointer) direct-declarator))

(defrule direct-declarator
    (and (? (or identifier
                (and |(| declarator |)|)))
         direct-declarator*))

(defrule direct-declarator*
    (* (or (and [ (? type-qualifier-list) (? assignment-expression) ])
           (and [ |static| (? type-qualifier-list) assignment-expression ])
           (and [ type-qualifier-list |static| assignment-expression ])
           (and [ (? type-qualifier-list) |*|) ; not #\] ?
           parameter-type-list
           (and |(| (? identifier-list) |)| ))))

(defrule pointer
    (and punctator-* (? type-qualifier-list) (? pointer))
  (:function rest))

(defrule type-qualifier-list
    (+ type-qualifier))

(define-bracket-rule parameter-type-list (punctuator-|(| punctuator-|)|)
    (or (and parameter-list punctuator-|,| punctuator-|...|)
        parameter-list))

(define-separator-list-rule parameter-list
  parameter-declaration punctator-|,|)

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
    (or pointer
        (and (? pointer) direct-abstract-declarator)))

(defrule direct-abstract-declarator
    (or (and |(| abstract-declarator |)|)
        (and (? direct-abstract-declarator)
             (or (and [ (? type-qualifier-list) (? assignment-expression) ])
                 (and [ |static| (? type-qualifier-list) assignment-expression ])
                 (and [ type-qualifier-list |static| assignment-expression ])
                 (and [ |*| ])
                 (and |(| (? parameter-type-list) |)|)))))

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
    (and (+ designator) |=|)
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
    (and |.| identifier)
  (:function second)
  (:lambda (member &bounds start end)
    (bp:node* (:member-designator :bounds (cons start end))
      (1 :member member))))

(defrule static_assert-declaration
    (and |_Static_assert| |(| constant-expression |,| string-listeral |)| punctuator-|;|))


;;; A.2.3 Statements

(defrule statement
    (or labeled-statement
        compound-statement
        expression-statement
        selection-statement
        iteration-statement
        jump-statement))

(defrule labeled-statement
    (or (and identifier punctuator-|:| statement)
        (and |case| constant-expression punctuator-|:| statement)
        (and |default| punctuator-|:| statement)))

(define-bracket-rule compound-statement (punctuator-{ punctuator-})
    (* block-item/?s))

(defrule block-item
    (or declaration
        statement))

(defrule block-item/?s
    (and block-item (* (or #\Space #\Tab #\Newline))) ; HACK
  (:function first))

(defrule expression-statement
    (and (? expression) (* (or #\Space #\Tab #\Newline)) |;|) ; HACK whitespace
  (:function first))

(defrule selection-statement
    (or if-statement switch-statement))

(defrule if-statement
    (and |if| |(expression)| statement (? (and |else| statement)))
  (:destructure (keyword1 test then &optional keyword2 else
                 &bounds start end)
    (declare (ignore keyword1 keyword2))
    (bp:node* (:if-statement :bounds (cons start end))
      (1    :test test)
      (1    :then then)
      (bp:? :else else))))

(defrule switch-statement
    (and |switch| expression/parentheses statement)
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
    (and |while| |(expression)| statement)
  (:function rest)
  (:destructure (keyword test body &bounds start end)
    (bp:node* (:while-statement :bounds (cons start end))
      (1 :test test)
      (1 :body body))))

(defrule do-statement
    (and |do| statement |while| |(expression)| |;|)
  (:destructure (keyword1 body keyword2 test semicolon &bounds start end)
    (declare (ignore keyword1 keyword2 semicolon))
    (bp:node* (:do-statement :bounds (cons start end))
      (1 :test test)
      (1 :body body))))

(defrule for-statement
    (and |for| |(|
         (or declaration (? expression)) |;| (? expression) |;| (? expression)
         |)| statement)
  (:destructure (keyword open init semi1 test semi2 step close body
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
             (and |break|)
             (and |return| (? expression)))
         punctator-|;|)
  (:function first))

(defrule goto-statement
    (and |goto| identifier)
  (:function second)
  (:lambda (label &bounds start end)
    (bp:node* (:goto-statement :label label :bounds (cons start end)))))

(defrule continue-statement
    |continue|
  (:lambda (keyword &bounds start end)
    (declare (ignore keyword))
    (bp:node* (:continue-statement :bounds (cons start end)))))


;;; A.2.4 External definitions

(defrule translation-unit/whitespace
    (and (* whitespace) translation-unit (* whitespace))
  (:function second))

(defrule translation-unit
    (+ external-declaration)
  (:lambda (declarations &bounds start end)
    (bp:node* (:translation-unit :bounds (cons start end))
      (* :declaration declarations))))

(defrule translation-unit*
    (or (and external-declaration translation-unit*) ; TODO
        (esrap:& (esrap:? character))))

(defrule external-declaration
    (or function-definition
        declaration))

(defrule function-definition
    (and declaration-specifiers declarator (* declaration) compound-statement)
  (:destructure (specifiers (pointer (name parameters)) declarations body &bounds start end)
    (bp:node* (:function-definition :name   name
                                    :bounds (cons start end))
      (* :return      specifiers)
      (1 :parameter   parameters)
      (* :declaration declarations)
      (* :body        body))))
(bp:with-builder ('list)
  (esrap:parse 'function-definition "int a(int x) { return 1; }"))
