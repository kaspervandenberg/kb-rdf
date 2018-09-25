;;;; trie/internal-generics

;;;; Definitions for internal generic functions, the method implementations of each generic are
;;;; defined in their own files.

;;;; © Kasper van den Berg, 2018


(in-package :net.kaspervandenberg.kb-rdf.trie)


(defgeneric find-intern (node key key-hash level path-to-parent)
  (:documentation
   "Search for `key` in `node` and its children.

Returns three values: the value in the SNode identified by `key` or nil, a truty value indicating
whether the key was found, and the path to the root."))


(defgeneric collect-subtree-values (node path-to-parent)
  (:documentation
   "Collect all key-value pairs containted in the subtree rooted at `node`.

Returns two values: a list of key-value pairs, and a list containing all the paths of all the nodes."))
