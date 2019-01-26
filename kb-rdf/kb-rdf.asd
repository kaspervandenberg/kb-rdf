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
	     (:file "nodes"
		    :depends-on ("package"))))))
