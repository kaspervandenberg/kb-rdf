;;;; trie/package

;;;; © Kasper van den Berg, 2018


;;; *.trie contains various Tries, i.e. hierarchical tree structures
(defpackage :net.kaspervandenberg.kb-rdf.trie
  (:use :common-lisp)
  (:import-from :net.kaspervandenberg.kb-rdf.bitindexed-list bucket-size))
