;;;; trie/cnode-children


;;;; © Kasper van den Berg, 2018


(in-package :net.kaspervandenberg.kb-rdf.trie)


(defun cnode-find-child (cnode key-hash level)
  "Find the CNode's child with the given key-hash.

Returns three values: the child, a boolean that indicates whether the child was found and the index
of the child is or could be in the CNode's branches."
  (let ((index (key-hash-to-index key-hash level)))
    (multiple-value-bind (child foundP)
	(net.kaspervandenberg.kb-rdf.bitindexed-list:find (get-branches cnode) index)
      (values child foundP index))))
