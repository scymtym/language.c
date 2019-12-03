;;;; evaluator.lisp --- Evaluation rules for nodes.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; This implements the semantics described in "6.10 Preprocessing
;;;; directives".

(cl:in-package #:language.c.preprocessor.evaluator)

;;; Lexical elements

(defmethod evaluate ((element     model::single-token-mixin)
                     (remainder   t)
                     (environment t))
  (values (list element) remainder))

(defmethod output ((token model::single-token-mixin) (target stream))
  (write-string (model::token-string token) target))

(defmethod evaluate ((element     model:identifier)
                     (remainder   t)
                     (environment t))
  (if-let ((macro (lookup (model:name element) environment)))
    (evaluate macro remainder environment)
    (values (list element) remainder)))

(defmethod output ((token model::header-name) (target stream))
  (multiple-value-bind (open close)
      (ecase (model:kind token)
        (:system (values #\< #\>))
        (:local  (values #\" #\")))
    (write-char open target)
    (write-string (model::token-string token) target)
    (write-char close target)))

;;;

(defmethod evaluate ((element     model:line)
                     (remainder   t)
                     (environment t))
  (multiple-value-bind (result remainder) (evaluate (model:tokens element) '() environment)
    (values (append result (list #\Newline)) remainder)))

;;; Group

(defmethod evaluate ((element     model:group)
                     (remainder   t)
                     (environment t))
  (values (mapcan (rcurry #'evaluate '() environment) (coerce (model:parts element) 'list)) ; TODO coerce
          remainder))

;;; 6.10.1 Conditional inclusion
(defmethod evaluate ((element     model:if*)
                     (remainder   t)
                     (environment t))
  (let* ((test-expression
           (evaluate-to-string (model:test element)
                               (make-instance 'test-environment :parent environment)))
         (test-ast
           (architecture.builder-protocol:with-builder ('list)
             (esrap:parse 'language.c.shared.parser::constant-expression (string-trim '(#\Space) test-expression)
                          )))) ; TODO do this without trimming
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
  (if (typep nodes '(cons model::header-name null)) ; TODO
      (first nodes)
      (let* ((string (evaluate-to-string nodes environment))
             (ast    (language.c.preprocessor.parser:parse ; TODO make a helper function for this
                      string (make-instance 'model:builder))))
        ast)))

(defmethod evaluate ((element     model:include)
                     (remainder   t)
                     (environment environment))
  (let* ((header-name (evaluate-header-name
                       (model:filename element) environment))
         (kind        (model:kind header-name))
         (name        (model:name header-name))
         (result      '()))
    (tagbody
     :retry
       (restart-case
           (let* ((filename (resolve-include environment kind name))
                  (content  (include filename environment)))
             (unless (eq content t)
               (push-file filename environment)
               (unwind-protect
                    (let ((ast (language.c.preprocessor.parser:parse
                                content (make-instance 'model:builder))))
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
(defmethod evaluate ((element     model:define-object-like-macro)
                     (remainder   t)
                     (environment environment))
  (let ((name        (model:name (model:name element)))
        (replacement (model:replacement element))) ; TODO make this more explicit?
    (setf (lookup name environment)
          (if (emptyp replacement)
              (make-instance 'empty-macro)
              (make-instance 'object-like-macro :replacement replacement))))
  (values '() remainder))

(defmethod evaluate ((element     model:define-function-like-macro)
                     (remainder   t)
                     (environment environment))
  (let ((name        (model:name (model:name element)))
        (parameters  (model:parameters element))
        (replacement (model:replacement element))) ; TODO make this more explicit?
    (setf (lookup name environment)
          (make-instance 'function-like-macro :parameters  parameters
                                              :replacement replacement)))
  (values '() remainder))

(defmethod evaluate ((element     model:undefine)
                     (remainder   t)
                     (environment environment))
  (let ((name (model:name (model:name element))))
    (remhash name (entries environment)))
  (values '() remainder))

(defmethod evaluate ((element     empty-macro)
                     (remainder   t)
                     (environment environment))
  (values '() remainder))

(defmethod evaluate ((element     object-like-macro)
                     (remainder   t)
                     (environment environment))
  (values '() (append (coerce (replacement element) 'list) remainder)))

(defmethod evaluate ((element     function-like-macro)
                     (remainder   t)
                     (environment t))
  (let ((parameters  (map 'list 'model:name (parameters element)))
        (replacement (replacement element)))
    (let* ((call-environment     (make-instance 'child-environment
                                                :parent environment))
           (argument-environment (make-instance 'argument-collecting-environment
                                                :call-environment call-environment
                                                :parameters       parameters
                                                :parent           environment)))
      (loop :with (first . rest) = remainder
            :for c = first :then (first new-remainder)
            :for r = rest :then (rest new-remainder)
            :for (result new-remainder done?)
               = (multiple-value-list (evaluate c r argument-environment))
            :do (print c)
            :while (and new-remainder (not done?))
            :finally (return (values (evaluate replacement '() (call-environment argument-environment)) new-remainder))))))

;;; 6.10.5, 6.10.6 Directives

(defmethod evaluate ((element     model:error*)
                     (remainder   t)
                     (environment environment))
  (error "not implemented"))

(defmethod evaluate ((element     model:pragma)
                     (remainder   t)
                     (environment environment))
  (let* ((first-token  (first (model::tokens element)))
         (token-string (model::token-string first-token)))
    (if-let ((which (find-symbol token-string '#:keyword)))
      (evaluate-pragma which element environment)
      (cerror "Ignore the directive" "Unknown directive ~A" token-string)))
  (values '() remainder))

(defmethod evaluate-pragma ((which       (eql :|once|))
                            (element     model:pragma)
                            (environment include-environment))
  (let ((current-file (first (include-stack environment))))
    (setf (gethash current-file (included-files environment)) t)))
