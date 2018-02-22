(defpackage :extended-types
  (:use :cl :enumerable)
  (:export make-var-expression
	   var-expression-name
	   var-expression-type
	   formp
	   type-structure-match
	   extended-typep
	   destructure-with-type
	   sub-type-with-bindings
	   make-grammar
	   make-compound-grammar
	   reverse-grammar))
(in-package :extended-types)

(defun make-var-expression (name &optional (type t))
  "Make a type specifier that specifies a variable of some type."
  (list 'var name type))

(defun var-expression-name (expression)
  (if (formp expression 'var)
      (cadr expression)
      (error "Expression is not a var-expression!")))

(defun var-expression-type (expression)
  (if (formp expression 'var)
      (if (> (length expression) 2)
	  (caddr expression)
	  t)
      (error "Expression is not a var-expression!")))

(defun formp (expression form)
  "Whether `expression' is a list whose first element is `form'."
  (and (typep expression 'list)
       (equalp (car expression) form)))

(defun type-structure-match (expression type-structure)
  "Whether `expression' has the type structure `type-structure'"
  (if (and type-structure expression)
      (and (extended-typep (car expression)
			   (car type-structure))
	   (extended-typep (cdr expression)
			   (cdr type-structure)))
      (equalp type-structure expression)))

;;;;;;; rememebr to add in "amb" forms for ambiguous type specification
(defun extended-typep (expression type-spec)
  "Whether `expression' is guaranteed to be of extended type specified by `type-spec'."
  (cond ((formp expression 'quote)
	 (let ((expression (cadr expression)))
	   (cond ((formp type-spec 'quote)
		  (equalp expression (cadr type-spec)))
		 ((formp type-spec 'var)
		  (extended-typep expression (var-expression-type type-spec)))
		 ((formp type-spec 'type-structure)
		  (if (typep expression 'list)
		      (type-structure-match expression (cadr type-spec))))
		 (t (typep expression type-spec)))))
	((formp expression 'var)
	 (extended-typep (var-expression-type expression) type-spec))
	((formp expression 'type-structure)
	 (if (formp type-spec 'type-structure)
	     (type-structure-match (cadr expression) (cadr type-spec))))
	(t (subtypep expression type-spec))))

(defun destructure-with-type (expression type-spec)
  "Perfom a destructuring-bind on `expression' according to the structure of `type-spec'.
Return the bindings as a hashmap."
  nil)

(defun sub-type-with-bindings (expression type-spec bindings)
  "Return a type-spec that is a sub-type of `type-spec' by replacing the variables in `type-spec' with the corresponding bindings in the hashmap `bindings'."
  nil)

(defun make-grammar (input-spec output-spec)
  "Return a dotted pair representing a grammar given type specifiers for the left and right side of the grammar."
  (cons input-spec output-spec))

(defun make-compound-grammar (&rest grammars)
  "Concatenate grammars."
  (make-grammar
   (make-compound-type-spec
    (map-yield #'car grammars))
   (make-compound-type-spec
    (map-yield #'cdr grammars))))

(defun reverse-grammar (grammar)
  "Reverse the direction of a grammar."
  (make-grammar (cdr grammar) (car grammar)))

;; also have a reg-exp type specifier which specifies strings that match some reg-exp

;; (deftype reg-exp-spec (reg-exp)
;;   `(and string
;; 	(satisfies reg-exp-spec reg-exp)))
