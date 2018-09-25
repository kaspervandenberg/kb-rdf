;;;; trie/collect-subtree-values

;; (defgeneric collect-subtree-values (node path-to-parent)
;;   (:documentation
;;    "Collect all key-value pairs containted in the subtree rooted at `node`.

;; Returns two values: a list of key-value pairs, and a list containing all the paths of all the nodes."))

;;;; © Kasper van den Berg, 2018


(in-package :net.kaspervandenberg.kb-rdf.trie)


(defmethod collect-subtree-values ((node CNode) path-to-parent)
  (multiple-value-bind (key-value-pairs paths)
      (mapcar-2values #'collect-subtree-values (get-branch-elements-list node))
    (values (append key-value-pairs)
	    (append paths))))


(defmethod collect-subtree-values ((node SNode) path-to-parent)
  (values (list (cons (get-key node) (get-value node)))
	  (list (cons node path-to-parent))))


(defmethod collect-subtree-values ((node INode) path-to-parent)
  (let ((m (get-main node)))
    (and m (collect-subtree-values m (cons node path-to-parent)))))
