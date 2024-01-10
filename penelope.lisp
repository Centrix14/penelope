(defpackage :penelope
  (:use :cl))

(in-package :penelope)

(defun ty (x &optional recursive)
  (typecase x
    (integer 'integer)
    (string 'string)
    (boolean 'boolean)
    (t (type-of x))))

(defun check-math (expr vars)
  (let* ((pattern '(integer integer))
         (match (map 'list #'equalp
                     (list (type-check (second expr) vars)
                           (type-check (third expr) vars))
                     pattern)))
    (values match
            pattern
            'integer)))

(defun check-comparison (expr vars)
  (let* ((pattern '(integer integer))
         (match (map 'list #'eql
                     (list (type-check (second expr) vars)
                           (type-check (third expr) vars))
                     pattern)))
    (values match
            pattern
            'boolean)))

(defun check-eq (expr vars)
  (let* ((pattern (list (type-check (second expr) vars)
                        (type-check (second expr) vars)))
         (match (map 'list #'eql
                     (list (type-check (second expr) vars)
                           (type-check (third expr) vars))
                     pattern)))
    (values match
            pattern
            'boolean)))

(defun check-if (expr &optional vars)
  (let* ((branch-type (if (member (third expr) vars)
                          (type-check (fourth expr) vars)
                          (type-check (third expr) vars)))
         (pattern (list 'boolean
                        branch-type
                        branch-type))
         (match (map 'list #'eql
                       (list (type-check (second expr) vars)
                             (type-check (third expr) vars)
                             (type-check (fourth expr) vars))
                       pattern)))
    (values match
            pattern
            (second pattern))))

(defun check-lambda (expr &optional vars)
  (let* ((pattern (list 'list
                        (type-check (third expr) vars)))
         (match (map 'list #'eql
                         (list (ty (second expr))
                               (type-check (third expr) vars))
                         pattern)))
    (values match
            pattern
            (second pattern))))

(defun check-apply (expr &optional vars)
  (let* ((lambda-types (map 'list #'ty (second (second expr))))
         (arglist-types (map 'list
                             (lambda (arg)
                               (type-check arg vars))
                             (third expr)))
         (match (map 'list #'eql
                           lambda-types
                           arglist-types)))
    (values match
            lambda-types
            (list lambda-types
                  (type-check (second expr) vars)))))

(defun type-check (expr vars)
  (if (listp expr)
      (multiple-value-bind (matching pattern expr-type)
          (case (first expr)
            ((+ - * /) (check-math expr vars))
            ((> <) (check-comparison expr vars))
            ((= !=) (check-eq expr vars))
            ((if) (check-if expr vars))
            ((lambda) (check-lambda expr vars))
            ((apply) (check-apply expr vars))
            (otherwise (error "Expression ~a has incorrect types" expr)))

        (map nil
             (lambda (result wish-type val)
               (if result
                   nil
                   (if (member val vars)
                       (format t "Expression ~a has type ~a | ~a~%"
                               val wish-type expr)
                       (error "Expression ~a must have type ~a | ~a~%"
                              val wish-type expr))))
             matching pattern (cdr expr))
        (values expr-type pattern))

      (ty expr t)))
