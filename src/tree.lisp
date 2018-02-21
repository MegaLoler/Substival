(defpackage :substival.tree
  (:use :cl :substival.util :enumerable)
  (:export recursive-enumerable-p
	   make-node
	   node-value
	   node-children
	   leafp
	   tree-to-list
	   map-tree-value
	   map-tree
	   tree-leaves))
(in-package :substival.tree)

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

(defun leafp (node)
  "Whether a node is a leaf or not."
  (not (any (node-children node))))

(defstruct (tree-node (:constructor make-node (value children)))
  (value nil :type t)
  (children nil :type enumerable))

(defmethod node-value ((node tree-node))
  (tree-node-value node))

(defmethod node-children ((node tree-node))
  (tree-node-children node))

(defun tree-to-list (node)
  "Convert a tree to a list given the root node.
Be careful not to use on infinite trees!"
  (declare (type recursive-enumerable node))
  (if (leafp node)
      (node-value node)
      (list (node-value node)
	    (mapcar #'tree-to-list (to-list (node-children node))))))

(defun map-tree-value (fn node)
  "Map `fn' to the values of each node of a tree."
  (declare (type recursive-enumerable node))
  (make-node (funcall fn (node-value node))
	     (map-yield (lambda (child)
	     		  (map-tree-value fn child))
	     		(node-children node))))

(defun map-tree (fn node)
  "Apply `fn' to each node of a tree."
  (declare (type recursive-enumerable node))
  (funcall fn node)
  (map-enumerable fn (node-children node)))

;; i want to implement it like this but maybe i can't yield from inside lambdas?

;; (defun tree-leaves (node)
;;   "Return an enumerable of the leaves of a tree."
;;   (declare (type recursive-enumerable node))
;;   (with-enumerable
;;     (map-tree (lambda (node) (if (leafp node) (yield (node-value node))))
;; 	      node)))

(defun tree-leaves (node)
  "Return an enumerable of the leaves of a tree."
  (declare (type recursive-enumerable node))
  (with-enumerable
    (if (leafp node) (yield (node-value node)))
    (do-enumerable (child (node-children node))
      (do-enumerable (leaf (funcall #'tree-leaves child))
	(yield leaf)))))

;; (tree-to-list (map-tree-value #'1+ (make-node 1 (list (make-node 2 nil) (make-node 3 nil)))))
;; (tree-to-list (make-node 1 (list (make-node 2 nil) (make-node 3 nil))))
;; (map-tree #'print (make-node 1 (list (make-node 2 nil) (make-node 3 nil))))
;; (map-tree (lambda (node) (if (leafp node) (print (node-value node))))
;; 	  (make-node 1 (list (make-node 2 nil) (make-node 3 nil))))
;; (make-node 1 (list (make-node 2 nil) (make-node 3 nil)))
;; (to-list (tree-leaves (make-node 1 (list (make-node 2 nil) (make-node 3 nil)))))




;; i also will need a way to traverse trees non-linearly, eg, with each branch alternatingly being traversed, to interweave it
