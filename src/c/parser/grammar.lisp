;;;; grammar.lisp --- Grammar for C18 (excluding the preprocessor language)
;;;;
;;;; Copyright (C) 2019 Daniel Kochmański
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Daniel Kochmański
;;;;         Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; https://web.archive.org/web/20181230041359if_/http://www.open-std.org/jtc1/sc22/wg14/www/abq/c17_updated_proposed_fdis.pdf

(cl:in-package #:language.c.c.parser)


;;; A.1.1 Lexical elements

(defrule token ; TODO never used
    (or keyword identifier constant string-literal punctuator))

;;; A.1.2 Keywords

(deftokens (keyword t :skippable whitespace* :requires-separation? t)
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

(defrule non-keyword-identifier ; TODO this rule will be a huge performance problem
    (and (! keyword) identifier)
  (:function second))

;;; A.2.1 Expression

(defrule primary-expression
    (or non-keyword-identifier
        constant
        string-literal
        expression/parentheses
        generic-selection))

(defrule primary-expression/?s
    (and primary-expression whitespace*)
  (:function first))

(defrule generic-selection
    (and keyword-|_Generic|
         punctuator-|(| assignment-expression punctuator-|,| generic-assoc-list punctuator-|)|))

(defrule generic-assoc-list
    (+ generic-association))

(defrule generic-association
    (and (or |default| type-name) punctuator-|:| assignment-expression))

;; TODO much of the postfix stuff is duplicated in the shared grammar

;;; Postfix expression use a slightly interesting pattern: for an
;;; input like "a[1]", the `postfix-expression' should return a
;;; `:subscript' node that has the node corresponding to "a" as one
;;; child and the node corresponding to "1" as the other child.
;;;
;;; This cannot be done in a bottom-up fashion in the
;;; `postfix-expression' rule without analyzing the AST for the
;;; postfix part. Such an analysis is inefficient and only possible if
;;; the builder that has been used to construct the AST supports the
;;; "unbuilding" operations.
;;;
;;; A better way is letting the various postfix kinds construct the
;;; AST directly. To achieve this, the rules for postfix parts return
;;; a function that accepts a node and returns the finished AST in
;;; which the passed node is a child. For a chain of such suffix
;;; parts, apply this pattern via `reduce' yields the desired AST.
(defrule postfix-expression
    (or (and type-name/parentheses brace-initializer postfix-expression*)
        (and primary-expression/?s (and)             postfix-expression*))
  (:destructure (subject initializer suffixes &bounds start)
    (let ((primary (if initializer
                       (error "not implemented")
                       subject)))
      (maybe-apply-suffix primary suffixes start))))

(defrule postfix-expression*
    (* (or subscript
           arguments
           member-access
           postfix-modify)))

(defrule subscript
    (and punctuator-[ expression punctuator-])
  (:function second)
  (:lambda (index &bounds start end)
    (declare (ignore start))
    (lambda (expression start)
      (bp:node* (:subscript :bounds (cons start end))
        (1 :expression expression)
        (1 :index      index)))))

(defrule arguments
    (and punctuator-|(| (? argument-expression-list) punctuator-|)|)
  (:function second)
  (:lambda (arguments &bounds start end)
    (declare (ignore start))
    (lambda (expression start)
      (bp:node* (:call :bounds (cons start end))
        (1 :function expression)
        (* :argument arguments)))))

(define-separator-list-rule argument-expression-list
  assignment-expression punctuator-|,|)

(defrule member-access
    (and (or punctuator-|.| punctuator-->) non-keyword-identifier)
  (:destructure (operator member &bounds start end)
    (declare (ignore start))
    (lambda (expression start)
      (bp:node* (:member-access :pointer (eq operator :|->|)
                                :bounds  (cons start end))
        (1 :expression expression)
        (1 :member     member)))))

(defrule postfix-modify
    (or punctuator-++ punctuator---)
  (:lambda (operator &bounds start end)
    (declare (ignore start))
    (lambda (expression start)
      (bp:node* (:unary-expression :operator operator
                                   :position :postfix
                                   :bounds   (cons start end))
        (1 :expression expression)))))

(defrule unary-expression
    (or (and punctuator-++      unary-expression)
        (and punctuator---      unary-expression)
        (and unary-operators    cast-expression)
        (and keyword-|sizeof|   (or type-name/parentheses unary-expression))
        (and keyword-|_Alignof| type-name/parentheses)
        (and (and)              postfix-expression))
  (:destructure (operator operand &bounds start end)
    (if operator
        (bp:node* (:unary-expression :operator operator :bounds (cons start end))
          (1 :operand operand))
        operand)))

(defrule unary-operators
    (or punctuator-& punctuator-* punctuator-+ punctuator-- punctuator-~ punctuator-!))

(defrule cast-expression
    (or (and type-name/parentheses cast-expression)
        (and (and)                 unary-expression))
  (:destructure (type expression &bounds start end)
    (if type
        (bp:node* (:cast-expression :bounds (cons start end))
          (1 :type       type)
          (1 :expression expression))
        expression)))

(defrule assignment-expression
    ;; TODO UNARY-EXPRESSION is not here in the original grammar
    (or proper-assignment-expression
        conditional-expression ; TODO CONDITIONAL-EXPRESSION is in shared grammar => no postfix-expression
        ; cast-expression
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
  (:destructure (first second &bounds start end)
    (if second
        (bp:node* (:expression-sequence :bounds (cons start end))
          (* :expression (list* first second)))
        first)))

(defrule expression-rest
    (and punctuator-|,| assignment-expression)
  (:function second))

(define-bracket-rule expression/parentheses (punctuator-|(| punctuator-|)|)
    expression)


;;; A.2.2 Declarations
;;;
;;; We deviate from the standard by defining distinct rules for the
;;; various kinds of declarations instead of allowing all possible
;;; combinations of specifiers and declarators and letting a later
;;; stage sort it out.

(defrule declaration
    (or type-declaration
        enum-declaration
        struct-or-union-declaration
        function-declaration
        variable-declaration
        declaration//
        static_assert-declaration))

;; TODO make a macro
(defrule type-declaration
    (and keyword-|typedef|
         ; TODO things like volatile
         (and type-specifier/?s (? init-declarator-list))
         punctuator-|;|)
  (:function second)
  (:destructure (type declarators &bounds start end)
    (bp:node* (:type-declaration :bounds (cons start end))
      (1 :type       type)
      (* :declarator declarators))))

(defrule enum-declaration
    (and enum-specifier punctuator-|;|)
  (:function first))

(defrule struct-or-union-declaration
    (and struct-or-union-specifier punctuator-|;|)
  (:function first))

(defrule function-declaration
    (and (? (or keyword-|extern| keyword-|static| function-specifier))
         (? type-specifier)
         direct-declarator
         punctuator-|;|)
  (:destructure (specifiers type declarator semicolon &bounds start end)
    (declare (ignore semicolon))
    (bp:node* (:function-declaration :specifiers specifiers
                                     :bounds     (cons start end))
      ; (bp:? :specifier  specifiers)
      (bp:? :type       type)
      (1    :declarator declarator))))

(defrule variable-declaration
    (and type-specifier/?s init-declarator-list punctuator-|;|)
  (:destructure (type declarators semicolon &bounds start end)
    (declare (ignore semicolon))
    (bp:node* (:variable-declaration :bounds (cons start end))
      (1 :type       type)
      (* :declarator declarators))))

(defrule declaration//
    (and declaration-specifiers/?s (? init-declarator-list) punctuator-|;|)
  (:destructure (specifiers inits semicolon &bounds start end)
    (declare (ignore semicolon))
    (bp:node* (:declaration :specifiers specifiers
                            :bounds     (cons start end))
      ; (* :specifier specifiers)
      (* :init      inits))))

(defrule/s (declaration-specifiers :s? nil :skippable?-expression whitespace*)
    (+ (or storage-class-specifier
           (and type-specifier (esrap:! punctuator-[))
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
    (and declarator/?s (? (and punctuator-|=| initializer)))
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

;;; We deviate from the (6.7.2) type-specifier rule in standard which
;;; is used in (6.7) declaration-specifiers to collect a "bag" of
;;; specifiers so that later stages have to detect invalid
;;; combinations. We use a `primitive-type' rule that only accepts
;;; valid combinations.
(defrule/s (type-specifier :s? nil :skippable?-expression whitespace*)
    (or primitive-type
        atomic-type-specifier
        struct-or-union-specifier
        enum-specifier
        typedef-name ; TODO uncomment and figure out how to make function-definition work despite it
        )
  #+no (:lambda (which &bounds start end)
         (bp:node* (:type-specifier :which which :bounds (cons start end)))))

(defrule primitive-type
    (or char-type
        integer-type
        double-type
        complex-type
        simple-primitive-type))

(defrule sign-type-modifier
    (or keyword-|signed| keyword-|unsigned|))

(defrule size-type-modifier
    (or keyword-|short| keyword-|long|))

(defrule char-type
    (and (? sign-type-modifier) keyword-|char|)
  (:function first)
  (:lambda (sign &bounds start end)
    (bp:node* (:primitive-type :which  :char
                               :sign   sign
                               :bounds (cons start end)))))

(defun parse-integer-type-modifiers (text start end)
  (multiple-value-bind (result position success?)
      (esrap:parse '(+ (or sign-type-modifier size-type-modifier keyword-|int|))
                   text :start start :end end :junk-allowed t)
    (flet ((fail (&optional format-control &rest format-arguments)
             (return-from parse-integer-type-modifiers
               (values result position
                       (if format-control
                           (apply #'format nil format-control format-arguments)
                           nil)))))
      (unless success?
        (fail))
      (let ((int?   nil)
            (sign   nil)
            (size   '()))
        (loop :for modifier :in result
              :do (ecase modifier
                    ((:int)
                     (when int?
                       (fail "Repeated int keyword."))
                     (setf int? t))
                    ((:signed :unsigned)
                     (when sign
                       (fail "Conflicting specifiers: ~(~A~) and ~(~A~)."
                             sign modifier))
                     (setf sign modifier))
                    (:short
                     (unless (null size)
                       (fail "Conflicting specifiers: ~{~(~A~)~^ ~} and ~(~A~)."
                             size modifier))
                     (push modifier size))
                    (:long
                     (unless (or (null size) (equal size '(:long)))
                       (fail "Conflicting specifiers: ~{~(~A~)~^ ~} and ~(~A~)."
                             size modifier))
                     (push modifier size))))
        (values (list sign size) position t)))))

(defrule integer-type
    (and ;; quick check and for error message
         (& (+ (or sign-type-modifier size-type-modifier keyword-|int|)))
         #'parse-integer-type-modifiers)
  (:function second)
  (:destructure (sign size &bounds start end)
    (bp:node* (:primitive-type :which  :integer
                               :sign   sign
                               :size   size
                               :bounds (cons start end)))))

(defrule double-type
    (or (and keyword-|double| keyword-|long|)
        (and keyword-|long| keyword-|double|)
        keyword-|double|)
  (:lambda (value &bounds start end)
    (bp:node* (:primitive-type :which  :double
                               :size   (if (consp value) :long nil)
                               :bounds (cons start end)))))

(defrule complex-type
    keyword-|_Complex|)

(defrule simple-primitive-type
    (or keyword-|void|
        keyword-|float|
        keyword-|_Bool|)
  (:lambda (which &bounds start end)
    (bp:node* (:primitive-type :which which :bounds (cons start end)))))

(defrule struct-or-union-specifier
    (and struct-or-union
         (or (and (? non-keyword-identifier) punctuator-{ struct-declaration-list punctuator-})
             (and non-keyword-identifier)))
  (:destructure (keyword (name &optional open body close) &bounds start end)
    (declare (ignore open close))
    (bp:node* (keyword :bounds (cons start end))
      (bp:? :name name)
      (*    :body body))))

(defrule struct-or-union
    (or keyword-|struct| keyword-|union|))

(defrule struct-declaration-list
    (+ struct-declaration))

(defrule struct-declaration
    (or struct-declaration/declarator
        static_assert-declaration))

(defrule struct-declaration/declarator
    (and (+ specifier-qualifier/?s) (* struct-declarator) punctuator-|;|)
  (:destructure (qualifiers declarators semicolon &bounds start end)
    (declare (ignore semicolon))
    (bp:node* (:struct-declarator :bounds (cons start end))
      (* :qualifier  qualifiers)
      (* :declarator declarators))))

(defrule/s (specifier-qualifier :s? nil :skippable?-expression whitespace*)
    (and (or type-specifier
             type-qualifier
             alignment-specifier)
         (esrap:! punctuator-[)) ; TODO can we avoid this?
  (:function first)
  #+no (:lambda (specifier &bounds start end)
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
    (and keyword-|enum| (or (and (? non-keyword-identifier) enum-body)
                            (and non-keyword-identifier)))
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
    (and non-keyword-identifier #|TODO enumeration-constant|# (? (and punctuator-|=| constant-expression)))
  (:destructure (name (&optional equals value) &bounds start end)
    (declare (ignore equals))
    (bp:node* (:enumerator :bounds (cons start end))
      (1    :name  name)
      (bp:? :value value))))

(defrule atomic-type-specifier
    (and keyword-|_Atomic| type-name/parentheses)
  (:function second)
  (:lambda (type)
    (error "TODO not implemented")))

(defrule type-qualifier
    (or keyword-|const| keyword-|restrict| keyword-|volatile| keyword-|_Atomic|))

(defrule function-specifier
    (or keyword-|inline| keyword-|_Noreturn|))

(defrule alignment-specifier
    (and keyword-|_Alignas| punctuator-|(| (or type-name constant-expression) punctuator-|)|)
  (:function third))

(defrule/s (declarator :s? nil :skippable?-expression whitespace*)
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
    (and (or non-keyword-identifier declarator/parentheses) direct-declarator*)
  (:destructure (name suffixes &bounds start end)
    (if nil ; TODO suffixes
        (bp:node* (:direct-declarator :bounds (cons start end))
          (1 :name   name)
          (* :suffix suffixes))
        name)))

(defrule direct-declarator*
    (* (or direct-declarator/bracket
           parameter-type-list
           identifier-list/parentheses)))

(defrule direct-declarator/bracket
    (and punctuator-[
         (or (and (? type-qualifier-list) (? assignment-expression))
             (and keyword-|static| (? type-qualifier-list) assignment-expression)
             (and type-qualifier-list keyword-|static| assignment-expression)
             (and (? type-qualifier-list) punctuator-*))
         punctuator-])
  (:function second))

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

(defrule parameter-type-list/maybe-empty
    (or parameter-type-list
        parameter-type-list/empty))

(defrule parameter-type-list/empty
    (and punctuator-|(| punctuator-|)|)
  (:constant '()))

(define-bracket-rule parameter-type-list (punctuator-|(| punctuator-|)|)
    (or (and parameter-list punctuator-|,| punctuator-|...|)
        parameter-list))

(define-separator-list-rule parameter-list
  parameter-declaration punctuator-|,|)

(defrule parameter-declaration
    (and declaration-specifiers/?s (or declarator
                                    (? abstract-declarator)))
  (:destructure (specifiers declarator &bounds start end)
    (bp:node* (:parameter-declaration :bounds (cons start end))
      (* :specifier  specifiers)
      (1 :declarator declarator))))

;;; `identifier-list' defined in shared grammar
;;; TODO maybe create a local definition that uses non-keyword-identifier?

(defrule type-name
    (and (+ specifier-qualifier) (? abstract-declarator))
  (:destructure (qualifiers declarator)
    (cond ((and (not declarator) (length= 1 qualifiers))
           (first qualifiers))
          (t
           (error "not implemented")))))

(define-bracket-rule type-name/parentheses (punctuator-|(| punctuator-|)|)
    type-name)

(defrule abstract-declarator
    (or pointer-list
        (and (? pointer-list) direct-abstract-declarator)))

(defrule direct-abstract-declarator
    (or (and punctuator-|(| abstract-declarator punctuator-|)|)
        (and (? direct-abstract-declarator)
             (or direct-abstract-declarator/bracket
                 parameter-type-list/maybe-empty))))

(defrule direct-abstract-declarator/bracket
    (and punctuator-[
         (or (and (? type-qualifier-list) (? assignment-expression) punctuator-])
             (and keyword-|static| (? type-qualifier-list) assignment-expression punctuator-])
             (and type-qualifier-list keyword-|static| assignment-expression punctuator-])
             (and punctuator-*))
         punctuator-])
  (:function second))

(defrule typedef-name
    (and non-keyword-identifier whitespace*)
  (:function first))

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
    (and punctuator-|.| non-keyword-identifier)
  (:function second)
  (:lambda (member &bounds start end)
    (bp:node* (:member-designator :bounds (cons start end))
      (1 :member member))))

(defrule static_assert-declaration
    (and keyword-|_Static_assert|
         punctuator-|(| constant-expression punctuator-|,| string-literal punctuator-|)|
         punctuator-|;|)
  (:destructure (keyword open expression comma message close semicolon
                         &bounds start end)
    (declare (ignore keyword open comma close semicolon))
    (bp:node* (:static-assert :bounds (cons start end))
      (1 :expression expression)
      (1 :message    message))))


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
             (and non-keyword-identifier))
         punctuator-|:| statement))

(defrule compound-statement
    block-items
  (:lambda (items &bounds start end)
    (bp:node* (:compound-statement :bounds (cons start end))
      (* :item items))))

(define-bracket-rule block-items (punctuator-{ punctuator-})
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
  (:destructure (keyword1 test then (&optional keyword2 else)
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
    (and keyword-|goto| non-keyword-identifier)
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
    (and (* (or keyword-|extern| keyword-|static| function-specifier)) ; TODO as nodes?
         (? type-specifier)
         ;; declaration-specifiers
         non-keyword-identifier #+was declarator parameter-type-list/maybe-empty #+was (* declaration) block-items)
  (:destructure (specifiers type declarator #+no (pointer (name parameters)) declarations body
                 &bounds start end)
    (bp:node* (:function-definition :specifiers specifiers
                                    :bounds     (cons start end))
      ;; (1 :declarator declarator)
      (1    :name        declarator)
      ; (*    :specifier   specifiers)
      (bp:? :return      type)
      ;; (* :parameter   parameters)
      ;; (*    :declaration declarations)
      (* :parameter      declarations)
      (*    :body        body))))
