;;;; trie/add-intern

;; (defgeneric add-intern (node key value key-hash level path-to-parent)
;;   (:documentation
;;    "Add `key`-`value` to the Trie.

;; Return three values: the possibly changed node that contains `key`-`value`; an action that the
;; parent node should execute if `add-intern` returned a node not `eq` to the parent's child; and, the
;; path to the root"))

;;;; © Kasper van den Berg, 2018


(in-package :net.kaspervandenberg.kb-rdf.trie)


(defmethod add-intern ((node SNode) key value key-hash level path-to-parent)
  (restart-case
      (if (not (equal key (get-key node)))
	  (let ((fresh-cnode (make-cnode)))
	    (flet ((add-node-to (container)
		     (add-intern container
				 (get-key node)
				 (get-value node)
				 (sxhash (get-key node))
				 level
				 path-to-parent))
		   (add-args-to (container)
		     (add-intern container
				 key
				 value
				 key-hash
				 level
				 path-to-parent)))
	      (add-args-to (add-node-to fresh-cnode))))
	  (error 'duplicate-key
		 :key key
		 :old-value (get-value node)
		 :new-value value
		 :parent path-to-parent))
    (keep-existing-value () node)
    (replace-value-with-new () )))
