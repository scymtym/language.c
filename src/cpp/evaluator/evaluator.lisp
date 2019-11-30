;;;; evaluator.lisp --- Evaluation rules for nodes.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; This implements the semantics described in "6.10 Preprocessing
;;;; directives".

(cl:in-package #:language.c.cpp.evaluator)

;;; Lexical elements

(defmethod evaluate ((element     model::single-token-mixin)
                     (environment t)
                     (target      stream))
  (write-string (model::token-string element) target)
  :done)

(defmethod evaluate ((element     model:identifier)
                     (environment t)
                     (target      stream))
  (expand (model:name element) environment))

;;;

(defmethod evaluate ((element     model:line)
                     (environment t)
                     (target      stream))
  (evaluate (model:tokens element) environment target)
  (terpri target)
  :done)

;;; Group

(defmethod evaluate ((element     model:group)
                     (environment t)
                     (target      t))
  (evaluate (model:parts element) environment target)
  :done)

;;;

(defmethod evaluate ((element     sequence) ; TODO put this and the environment into constant-expression.lisp?
                     (environment undefined-is-0-environment)
                     (target      stream))
  (call-next-method)
  #+no (let ((first?   t)
        (defined? nil))
    (map nil (named-lambda node (node)
               (when (and (typep node 'model:identifier)
                          (string= (model:name node) "defined"))
                 (setf defined? t)
                 (return-from node))
               (if first?
                   (setf first? nil)
                   (write-char #\Space target))
               (cond (defined?
                      (let ((macro (lookup (model:name node) environment)))
                        (write-string (if macro "1" "0") target))
                      (setf defined? nil))
                     (t
                      (evaluate node environment target))))
         element)))

;;; 6.10.1 Conditional inclusion
(defmethod evaluate ((element     model:if*)
                     (environment t)
                     (target      t))
  (let* ((test-expression
           (evaluate-to-string (model:test element)
                               (make-instance 'undefined-is-0-environment :parent environment)))
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
      (evaluate successor environment target)))
  :done)

;;; Control lines

;;; 6.10.2 Source file inclusion
(defun evaluate-header-name (nodes environment)
  (if (typep nodes '(cons model::header-name null)) ; TODO
      (first nodes)
      (let* ((string (evaluate-to-string nodes environment))
             (ast    (language.c.cpp.parser:parse
                      string (make-instance 'model:builder))))
        ast)))

(defmethod evaluate ((element     model:include)
                     (environment environment)
                     (target      t))
  (let* ((header-name (evaluate-header-name
                       (model:filename element) environment))
         (kind        (model:kind header-name))
         (name        (model:name header-name)))
    (tagbody
     :retry
       (restart-case
           (let* ((filename (resolve-include environment kind name))
                  (content  (include filename environment)))
             (unless (eq content t)
               (push-file filename environment)
               (unwind-protect
                    (let ((ast (language.c.cpp.parser:parse
                                content (make-instance 'model:builder))))
                      (evaluate ast environment target)
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
       (return-from evaluate :done))))

;;; 6.10.3 Macro replacement
(defmethod evaluate ((element     model:object-like-macro)
                     (environment environment)
                     (target      t))
  (let ((name        (model:name (model:name element)))
        (replacement (model:replacement element))) ; TODO make this more explicit?
    (setf (lookup name environment)
          (if (emptyp replacement)
              (make-instance 'empty-macro)
              (make-instance 'object-like-macro :replacement replacement))))
  :done)

(defmethod evaluate ((element     model:function-like-macro)
                     (environment environment)
                     (target      t))
  (let ((name        (model:name (model:name element)))
        (parameters  (model:parameters element))
        (replacement (model:replacement element))) ; TODO make this more explicit?
    (setf (lookup name environment)
          (make-instance 'function-like-macro :parameters  parameters
                                              :replacement replacement)))
  :done)

(defmethod evaluate ((element     model:undefine)
                     (environment environment)
                     (target      t))
  (let ((name (model:name (model:name element))))
    (remhash name (entries environment)))
  :done)

(defmethod evaluate ((element     model:error*)
                     (environment environment)
                     (target      t))
  (error "not implemented"))

(defmethod evaluate ((element     model:pragma)
                     (environment environment)
                     (target      t))
  (let* ((first-token  (first (model::tokens element)))
         (token-string (model::token-string first-token)))
    (if-let ((which (find-symbol token-string '#:keyword)))
      (evaluate-pragma which element environment target)
      (cerror "Ignore the directive" "Unknown directive ~A" token-string)))
  :done)

(defmethod evaluate-pragma ((which       (eql :|once|))
                            (element     model:pragma)
                            (environment include-environment)
                            (target      t))
  (let ((current-file (first (include-stack environment))))
    (setf (gethash current-file (included-files environment)) t))
  :done)
