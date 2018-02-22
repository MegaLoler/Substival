(defsystem #:substival
  :description "A bidirectional substitution evaluator."
  :author "MegaLoler"
  :serial t
  :depends-on (#:enumerable)
  :components ((:module "src"
			:serial t
			:components
			((:file "extended-types")
			 (:file "substival")))))
