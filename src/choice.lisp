(defpackage :substival.choice
  (:use :cl :substival.util :substival.tree :enumerable :cl-arrows)
  (:export choice-point
	   state
	   choices
	   states
	   choices-states
	   choose
	   where-state
	   choose-nth
	   choose-nth-state
	   choose-first
	   choose-second
	   choose-third
	   choose-fourth
	   choose-fifth
	   choose-sixth
	   choose-seventh
	   choose-eighth
	   choose-ninth
	   choose-tenth
	   choose-first-state
	   choose-second-state
	   choose-third-state
	   choose-fourth-state
	   choose-fifth-state
	   choose-sixth-state
	   choose-seventh-state
	   choose-eighth-state
	   choose-ninth-state
	   choose-tenth-state
	   node-value
	   node-children))
(in-package :substival.choice)

;; i'm thinking abut.... simply defining a choice point as an enumerable that enumerates over its children? idk

;; a structure to represent a nondeterministic choice point
;; has the current state and an enumeration of choices
;; which may or may not be further choice points
(defstruct (choice-point (:constructor choice-point (state choices))
			 (:conc-name nil))
  (state nil :type t)
  (choices nil :type enumerable))

(defun states (choice-points)
  "Get the states of an enumeration of choice points."
  (map-yield #'state choice-points))

(defun choices-states (choice-point)
  "Get the states of the choices of a choice point."
  (states (choices choice-point)))
       
(defmacro choose (choice-point &body body)
  "Arrow notation on the choosable states at a choice point."
  `(-> (choices ,choice-point) ,@body))

(defmacro where-state (enumerable-choice-points predicate)
  "Filter an enumeration of choice points by applying a predicate to their states."
  `(where ,enumerable-choice-points
	  (lambda (choice-point)
	    (funcall ,predicate (state choice-point)))))

(defun choose-nth (choice-point n)
  "Return the nth choice at a choice point."
  (element-at (choices choice-point) n))

(defun choose-nth-state (choice-point n)
  "Return the state of nth choice at a choice point."
  (state (element-at (choices choice-point) n)))

(defmacro def-choose-nth (nth-name n)
  "Define a function of the form choose-first choose-second etc.."
  `(defun ,(intern (format nil "~A-~A"
			   (symbol-name 'choose)
			   (symbol-name nth-name)))
       (choice-point)
     (choose-nth choice-point ,n)))

(defmacro def-choose-nth-state (nth-name n)
  "Define a function of the form choose-first-state choose-second-state etc.."
  `(defun ,(intern (format nil "~A-~A-~A"
			   (symbol-name 'choose)
			   (symbol-name nth-name)
			   (symbol-name 'state)))
       (choice-point)
     (choose-nth-state choice-point ,n)))

(def-choose-nth first 0)
(def-choose-nth second 1)
(def-choose-nth third 2)
(def-choose-nth fourth 3)
(def-choose-nth fifth 4)
(def-choose-nth sixth 5)
(def-choose-nth seventh 6)
(def-choose-nth eighth 7)
(def-choose-nth ninth 8)
(def-choose-nth tenth 9)
(def-choose-nth-state first 0)
(def-choose-nth-state second 1)
(def-choose-nth-state third 2)
(def-choose-nth-state fourth 3)
(def-choose-nth-state fifth 4)
(def-choose-nth-state sixth 5)
(def-choose-nth-state seventh 6)
(def-choose-nth-state eighth 7)
(def-choose-nth-state ninth 8)
(def-choose-nth-state tenth 9)

;; implement choice-point as a recursive-enumerable
(defmethod node-value (choice-point)
  "Return the state of the choice point as the value of this node in the tree of choices."
  (state choice-point))

(defmethod node-children (choice-point)
  "Return the choices of the choice point as the children of this node in the tree fo choices."
  (choices choice-point))
