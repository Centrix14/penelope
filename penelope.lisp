(defpackage :penelope
  (:use :cl))

(in-package :penelope)

(defun ty (x)
  (typecase x
    (integer 'integer)
    (string 'string)
    (boolean 'boolean)
    (t (type-of x))))

(defvar *env* (make-hash-table))

(map 'list
     (lambda (pair)
       (let ((variable (first pair))
             (type (second pair)))
         (setf (gethash variable *env*) type)
         pair))
     '((age integer)
       (name string)
       (+ math-op)
       (- math-op)
       (* math-op)
       (/ math-op)
       (> math-op)
       (< math-op)
       (= eq)
       (if if)
       (lambda lambda)))

(defun if-typing (expr)
  (if (and (eql (first expr) 'if)
           (eql (second expr) 'boolean)
           (eql (third expr) (fourth expr)))
      (third expr)
      nil))

(defvar *rules*
  `(((math-op integer integer) integer)
    ((eq integer integer) boolean)
    ,#'if-typing))

(defun get-type-from-rule (expr rules)
  (if (null rules)
      (error "Expression ~a has incorrect types~%" expr)
      (let ((rule (car rules)))
        (if (functionp rule)
            (if (funcall rule expr)
                (funcall rule expr)
                (get-type-from-rule expr (cdr rules)))
            (if (equalp (first rule) expr)
                (second rule)
                (get-type-from-rule expr (cdr rules)))))))

(defun type-check (rules env expr)
  (if (listp expr)
      (get-type-from-rule
       (map 'list (lambda (x)
                    (type-check rules env x))
            expr)
       rules)
      (let ((type (gethash expr env)))
        (if type
            type
            (ty expr)))))
