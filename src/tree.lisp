(defpackage :substival.tree
  (:use :cl :substival.util :enumerable)
  (:export recursive-enumerable-p
	   make-node
	   node-value
	   node-children
	   leafp
	   map-tree))
(in-package :substival.tree)

;;;;; things to do....
;;;;; enumerate leaves... linearly
;;;;; enumerate leaves... interwovenly
;;;;; enumerate leaves... randomly?
;;;;; re-enumeration which produces an enumerable which enumerables an enumerable in a different order?

(defun recursive-enumerable-p (x)
  "Is an object recursively enumerable?"
  (and
   (compute-applicable-methods #'node-value (list x))
   (compute-applicable-methods #'node-children (list x))))

(deftype recursive-enumerable ()
  "An enumerable tree."
  '(satisfies recursive-enumerable-p))

(defgeneric node-value (recursive-enumerable)
  (:documentation "Return the value of a tree node."))

(defgeneric node-children (recursive-enumerable)
  (:documentation "Return an enumeration of the children of a tree node."))

(defclass node ()
  ((value :accessor node-value
	  :initarg :value
	  :type t)
   (children :accessor node-value
	     :initarg :children
	     :type enumerable))) ;; more specifically, an enumerable of recursive enumerables

(defun make-node (value children)
  (make-instance 'node :value value :children children))

(defmethod node-value ((node node))
  (node-value node))

(defmethod node-children ((node node))
  (node-children node))

(defun leafp (node)
  "Whether a node is a leaf or not."
  (null (node-children node)))

;; i want to be able to map a function to every node in a tree
;; and that function has to return a recursive enumerable of course
(defun map-tree (fn node)
  "Map `fn' to each node of a tree."
  (declare (type recursive-enumerable node))
  (make-node (funcall fn (node-value node))
	     (map-yield (lambda (child)
			  (map-tree fn child))
			(node-children node))))

;(map-tree #'1+ (make-node 1 (list (make-node 2 nil) (make-node 3 nil))))

;; and i want a function that makes an enumerable from the leaves 

;; and i want a `re-enumerate` that takes an enumerable and reorders it some way
;; maybe... not sure honestly
;; basically.... i need to decide how to exhaust choices in infinite trees..
;; yes, basicaly i just need to think about.... How do i want to explore the trees?
;; what order ?

;;(defvar
