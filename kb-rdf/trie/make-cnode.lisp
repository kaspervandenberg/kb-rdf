;;;; trie/make-cnode

;; (defmethod make-cnode (node)
;;   :documentation
;;   "Factory method to make the correct kind of CNode.

;; Creates an empty CNode, use `add-intern` to add values to it.")

;;;; © Kasper van den Berg, 2018


(in-package :net.kaspervandenberg.kb-rdf.trie)


(defmethod make-cnode ((node Immutable-Trie-Mixin))
  (make-instance 'CNode-ro))


(defmethod make-cnode ((node Concurrent-Trie-Mixin))
  (make-instance 'CNode-c))
