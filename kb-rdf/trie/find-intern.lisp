;;;; trie/find-intern

;; (defgeneric find-intern (node key key-hash level path-to-parent)
;;   (:documentation
;;    "Search for `key` in `node` and its children.

;; Returns three values: the value in the SNode identified by `key` or nil, a truty value indicating
;; whether the key was found, and the path to the root."))

;;;; © Kasper van den Berg, 2018


(in-package :net.kaspervandenberg.kb-rdf.trie)


(defmethod find-intern ((node SNode) key key-hash level path-to-parent)
  (if (equal key (get-key node))
      (values (get-value node)
	      t
	      (cons node path-to-parent))
      (values nil
	      nil
	      path-to-parent)))


(defmethod find-intern ((node CNode) key key-hash level path-to-parent)
  (multiple-value-bind (child foundP) (cnode-find-child node key-hash level)
    (if foundP
	(find-intern child key key-hash (1+ level) (cons node path-to-parent))
	(values child
		foundP
		path-to-parent))))


(defmethod find-intern ((node INode) key key-hash level path-to-parent)
  (let ((m (get-main node)))
    (if m
	(find-intern m key key-hash level path-to-parent)
	(error 'dangling-inode))))
