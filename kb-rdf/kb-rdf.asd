;;;; kb-rdf system

;;;; © Kasper van den Berg, 2018

(defsystem "kb-rdf"
  :components
  ((:module bitindexed-list
	    :components
	    ((:file "package")
	     (:file "bitindexed-list"
		    :depends-on ("package"))))
   (:module trie
	    :depends-on ("bitindexed-list")
	    :components
	    ((:file "package")
	     (:file "nodes"
		    :depends-on ("package"))
	     (:file "keys"
		    :depends-on ("package"))))))
