;;;; evaluator.lisp --- Evaluation rules for nodes.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; This implements the semantics described in "6.10 Preprocessing
;;;; directives".

(cl:in-package #:language.c.preprocessor.evaluator)

;;; Output

(defmethod output ((token model::single-token-mixin) (target stream))
  (write-string (model::token-string token) target))

(defmethod output ((token model::string-literal) (target stream))
  (write-char #\" target)
  (write-string (model::token-string token) target)
  (write-char #\" target))

(defmethod output ((token model:header-name) (target stream))
  (multiple-value-bind (open close) ; TODO do this in token->string?
      (ecase (model:kind token)
        (:system (values #\< #\>))
        (:local  (values #\" #\")))
    (write-char open target)
    (write-string (model::token-string token) target)
    (write-char close target)))

;;; Lexical elements

(defmethod evaluate ((element     model::single-token-mixin)
                     (remainder   t)
                     (environment t))
  (values (list element) remainder))

(defmethod evaluate ((element     model:identifier)
                     (remainder   t)
                     (environment t))
  (if-let ((macro (lookup (model:name element) environment)))
    ;; ELEMENT names a macro MACRO which we evaluate to get its
    ;; EXPANSION and a new REMAINDER. The expansion will be
    ;; "re-scanned" by our caller according to the rules in 6.10.3.4
    ;; Rescanning and further replacement. If ELEMENT occurs in
    ;; EXPANSION (for example "foo" expanding to "1 foo 2"), this
    ;; occurrence must not be evaluated as a macro. To allow our
    ;; caller to do this, we return a third value which is
    ;;
    ;;   (ELEMENT . END)
    ;;
    ;; where END is (lastcar REMAINDER). The caller can use this to
    ;; not evaluate ELEMENT as a macro until it has scanned beyond
    ;; END.
    (multiple-value-bind (expansion remainder)
        (evaluate macro remainder environment)
      (cond ((eq expansion :not-expanded)
             (values (list element) remainder))
            ((null expansion)
             (values '() remainder))
            ((typep macro 'object-like-macro) ; TODO macro should indicate this
             (values '() (append expansion remainder) (cons element (lastcar expansion))))
            (t
             (values '() (append expansion remainder)))))
    (values (list element) remainder)))

;;;

(defmethod evaluate ((element     model:line)
                     (remainder   t)
                     (environment t))
  (let ((new-remainder (list* (make-instance 'model::punctuator :which #\Newline) remainder)))
    (evaluate (model:tokens element) new-remainder environment))
  #+old (multiple-value-bind (result remainder) (evaluate (model:tokens element) '() environment)
          (values (append result (list #\Newline)) remainder)))

;;; Group

(defmethod evaluate ((element     model:group)
                     (remainder   t)
                     (environment t))
  (let ((flat (mappend (lambda (part)
                         (typecase part
                           (model:line
                            (append (coerce (model:tokens part) 'list)
                                    (list (make-instance 'model::punctuator :which #\Newline))))
                           (t
                            (list part))))
                       (coerce (model:parts element) 'list))))
    (evaluate flat remainder environment))
  #+no (evaluate (coerce (model:parts element) 'list) remainder environment)
  #+no (values (mapcan (rcurry #'evaluate '() environment) (coerce (model:parts element) 'list)) ; TODO coerce
          remainder))

;;; 6.10.1 Conditional inclusion

(defun evaluate-test/if (element environment)
  (let* ((test-expression
           (evaluate-to-string (model:test element)
                               (make-instance 'test-environment :parent environment)))
         (test-ast
           (architecture.builder-protocol:with-builder ('list)
             (language.c.preprocessor.parser::%parse
              'language.c.shared.parser:constant-expression (string-trim '(#\Space) test-expression))))) ; TODO do this without trimming
    (eval-constant-expression test-ast)))

(defun evaluate-test/ifdef (element environment kind)
  (let* ((name     (model:name (aref (model:test element) 0))) ; TODO make this robust
         (defined? (lookup name environment)))
    (ecase kind
      (:ifdef  defined?)
      (:ifndef (not defined?)))))

(defmethod evaluate ((element     model:if*)
                     (remainder   t)
                     (environment t))
  (let* ((kind      (model:kind element))
         (result    (ecase kind
                      (:if
                       (evaluate-test/if element environment))
                      ((:ifdef :ifndef)
                       (evaluate-test/ifdef element environment kind))))
         (successor (if result
                        (model:then element)
                        (model:else element))))
    (evaluate successor remainder environment))

  #+no (let* ((test-expression
                (evaluate-to-string (model:test element)
                                    (make-instance 'test-environment :parent environment)))
              (test-ast
                (architecture.builder-protocol:with-builder ('list)
                  (language.c.preprocessor.parser::%parse
                   'language.c.shared.parser:constant-expression (string-trim '(#\Space) test-expression))))) ; TODO do this without trimming
                                        ; (evaluate test-ast environment target)
         (let* ((test-result (eval-constant-expression test-ast))
                (result      (ecase (model:kind element)
                               (:if     test-result)
                               (:ifdef  test-result)
                               (:ifndef (not test-result))))
                (successor   (if result
                                 (model:then element)
                                 (model:else element))))
           (evaluate successor remainder environment))))

;;; Control lines

;;; 6.10.2 Source file inclusion
(defun evaluate-header-name (nodes environment)
  (if (typep nodes '(cons model:header-name null)) ; TODO
      (first nodes)
      (let* ((string (evaluate-to-string nodes environment))
             (ast    (language.c.preprocessor.parser:parse ; TODO make a helper function for this
                      string (make-instance 'model:builder)
                      :rule 'language.c.preprocessor.parser::header-name)))
        ast)))

(defmethod evaluate ((element     model::include-base)
                     (remainder   t)
                     (environment environment))
  (let* ((header-name (evaluate-header-name
                       (coerce (model:filename element) 'list) environment)) ; TODO without coerce
         (kind        (cond ((typep element 'model::include-next)
                             :system/next)
                            (t
                             (model:kind header-name))))
         (name        (model:name header-name))
         (result      '()))
    (tagbody
     :retry
       (restart-case
           (let* ((resolved (resolve-include kind name environment))
                  (filename (file resolved))
                  (content  (include filename environment)))
             (format *trace-output* "// Including ~A~%" filename)
             (unless (eq content t)
               (push-file resolved environment)
               (unwind-protect
                    (let ((ast (language.c.preprocessor.parser:parse
                                content (make-instance 'model:builder)))) ; TODO reuse builder
                      (setf result (evaluate ast '() environment))
                      (go :done))
                 (pop-file environment))))
         (continue (&optional condition)
           :report (lambda (stream)
                     (format stream "~@<Ignore the ~A include ~S.~@:>"
                             kind name))
           (declare (ignore condition))
           (go :done))
         (retry ()
           :report (lambda (stream)
                     (format stream "~@<Retry processing the ~A ~
                                     include ~S.~@:>"
                             kind name))
           ;; TODO reset environment
           (go :retry)))
     :done
       (return-from evaluate (values result remainder)))))

;;; 6.10.3 Macro replacement
;;;
;;; The first three methods are for object-like and function-like
;;; macro definition and undefinition, the rest are for macro
;;; replacement.

(macrolet ((with-macro-definition ((name-var element remainder) &body body)
             `(let ((,name-var (model:name (model:name ,element))))
                ,@body
                (values '() ,remainder))))

  (defmethod evaluate ((element     model:define-object-like-macro)
                       (remainder   t)
                       (environment t))
    (with-macro-definition (name element remainder)
      (setf (lookup name environment)
            (let ((replacement (model:replacement element)))
              (if (emptyp replacement)
                  (make-instance 'empty-macro)
                  (make-instance 'object-like-macro
                                 :replacement replacement))))
      #+no (format t "// Defining object-like macro \"~A\" -> ~A~%"
              name (lookup name environment))))

  (defmethod evaluate ((element     model:define-function-like-macro)
                       (remainder   t)
                       (environment t))
    (with-macro-definition (name element remainder)
      (setf (lookup name environment)
            (let ((parameters  (map 'list #'model:name
                                    (model:parameters element)))
                  (replacement (model:replacement element)))
              (make-instance 'function-like-macro :parameters  parameters
                                                  :replacement replacement)))
      #+no (format t "// Defining function-like macro \"~A\" -> ~A~%"
              name (lookup name environment))))

  (defmethod evaluate ((element     model:undefine)
                       (remainder   t)
                       (environment t))
    (with-macro-definition (name element remainder)
      (remhash name (entries environment)))))

(defmethod evaluate ((element     empty-macro)
                     (remainder   t)
                     (environment t))
  (values '() remainder))

(defmethod evaluate ((element     object-like-macro)
                     (remainder   t)
                     (environment t))
  (values (coerce (replacement element) 'list) remainder))

(defmethod evaluate ((element     function-like-macro)
                     (remainder   t)
                     (environment t))
  (unless (and (typep (first remainder) 'model::punctuator)
               (eq (model::which (first remainder)) :|(|))
    (return-from evaluate :not-expanded))
  (loop :with argument-environment = (make-argument-collecting-environment
                                      (parameters element) environment)
        :for (first . rest) = remainder :then new-remainder
        :for (result new-remainder done?)
           = (multiple-value-list (evaluate first rest argument-environment))
        :while (and new-remainder (not done?))
        :finally (let* ((environment (call-environment argument-environment))
                        (replacement (replacement element))
                        (expansion   (evaluate replacement '() environment)))
                   (return (values expansion new-remainder)))))

#+no (defmethod evaluate ((element     model::punctuator)
                     (remainder   t)
                     (environment t))
  (case (model::which element)
    (:|#|  (destructuring-bind (&optional first &rest rest) remainder
             (cond ((not first)
                    (error "identifier must follow #"))
                   ((not (typep first 'model:identifier))
                    (error "identifier must follow #"))
                   (t
                    (let* ((name   (model:name first))
                           (macro  (lookup name environment))
                           (string (evaluate-to-string macro (make-instance 'environment))))
                      (values (list (make-instance 'model::string-literal
                                                   :value string))
                              rest))))))
    (:|##| (values '(:join) remainder))
    (t     (call-next-method))))

#+no (defmethod evaluate ((element (eql :join)) (remainder t) (environment t))
  (values '(:join) remainder))

(defmethod evaluate ((element     model::stringification)
                     (remainder   t)
                     (environment t))
  (let* ((parameter (model::parameter element))
         ; (string    (evaluate-to-string (list parameter) environment))
         (name      (model:name parameter))
         (macro     (lookup name environment))
         (string    (evaluate-to-string macro (make-instance 'environment)))
         ) ; TODO do not cons an environment
    (values (list (make-instance 'model::string-literal :value string))
            remainder)))

(defmethod evaluate ((element     model::concatenation)
                     (remainder   t)
                     (environment t))
  (let* ((left   (multiple-value-bind (result remainder)
                     (evaluate (list (model::left element)) () environment)
                   (append result remainder)))
         (right  (multiple-value-bind (result remainder)
                     (evaluate (list (model::right element)) () environment)
                   (append result remainder)))
         (string (with-output-to-string (stream)
                   (output (lastcar left) stream)
                   (output (first right)  stream))))
    (values ()
            (append (list (butlast left))
                    (list (make-instance 'model::identifier :name string))
                    (rest right)
                    remainder)          ; TODO make a class
            )))

;;; 6.10.5, 6.10.6 Directives

(defmethod evaluate ((element     model:error*)
                     (remainder   t)
                     (environment t))
  (cerror "Ignore the error" "~{~A~^ ~}"
          (map 'list (rcurry #'evaluate-to-string environment)
               (model:message element))))

(defmethod evaluate ((element     model:pragma)
                     (remainder   t)
                     (environment t))
  (let* ((first-token  (first-elt (model:tokens element)))
         (token-string (model::token-string first-token)))
    (if-let ((which (find-symbol token-string '#:keyword)))
      (evaluate-pragma which element environment)
      (cerror "Ignore the directive." "Unknown directive ~A" token-string)))
  (values '() remainder))

(defmethod evaluate-pragma ((which       (eql :|once|))
                            (element     model:pragma)
                            (environment include-environment))
  (let ((current-file (first (include-stack environment))))
    (setf (gethash current-file (included-files environment)) t)))

;;; Non-standard directives

(defmethod evaluate-pragma ((which       (eql :|GCC|))
                            (element     model:pragma)
                            (environment include-environment)))
