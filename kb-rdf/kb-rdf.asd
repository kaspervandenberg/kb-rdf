;;;; kb-rdf system


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
		    :depends-on ("internal-generics" "conditions" "cnode-children"))
	     (:file "cnode-children"
		    :depends-on ("package"))))))
