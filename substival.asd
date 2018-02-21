(defsystem #:substival
  :description "A bidirectional nondeterministic substitution evaluator."
  :author "MegaLoler"
  :serial t
  :depends-on (#:enumerable
	       #:extended-type-specifiers
	       #:cl-arrows)
  :components ((:module "src"
			:serial t
			:components
			((:file "util")
			 (:file "tree")
			 (:file "choice")
			 (:file "substival")))))
