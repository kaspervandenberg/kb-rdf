;;;; kb-rdf system

;;;; © Kasper van den Berg, 2018

(defsystem "kb-rdf"
  :components
  ((:module bitindexed-list
	    :components
	    ((:file "bitindexed-list")))
   (:module trie
	    :depends-on ("bitindexed-list")
	    :components
	    ((:file "package")
	     (:file "constants"
		    :depends-on ("package"))
	     (:file "nodes"
		    :depends-on ("package"))
	     (:file "conditions"
		    :depends-on ("package"))
	     (:file "internal-generics"
		    :depends-on ("package"))
	     (:file "find-intern"
		    :depends-on ("internal-generics"
				 "nodes"
				 "conditions"
				 "cnode-children"))
	     (:file "collect-subtree-values"
		    :depends-on ("internal-generics"
				 "nodes"
				 "cnode-children"
				 "util"))
	     (:file "add-intern"
		    :depends-on ("internal-generics"
				 "nodes"
				 "factory-methods"))
	     (:file "cnode-children"
		    :depends-on ("package"
				 "nodes"))
	     (:file "factory-methods"
		    :depends-on ("nodes"))
	     (:file "util"
		    :depends-on ("package"))))))
