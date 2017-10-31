(defun create-Triple (subj pred object)
  (list subj pred object))

(defun subject (triple)
  (first triple))

(defun predicate (triple)
  (second triple))

(defun object (triple)
  (third triple))

(defun subject-predicate (triple)
  `(,(subject triple) ,(predicate triple)))

(defun subject-object (triple)
  `(,(subject triple) ,(object triple)))

(defun predicate-object (triple)
  `(,(predicate triple) ,(object triple)))



;; Demo triples
(defvar s1 "http://example.org/uri/s1")
(defvar s2 "http://example.org/uri/s2")
(defvar s3 "http://example.org/uri/s3")

(defvar p1 "http://example.org/uri/p1")
(defvar p2 "http://example.org/uri/p2")

(defvar kasper "http://example.org/uri/kasper")
(defvar simone "http://example.org/uri/simone")
(defvar anneroos "http://example.org/uri/anneroos")

(defvar kind "http://example.org/uri/kind")
(defvar ouder "http://example.org/uri/ouder")
(defvar partner "http://example.org/uri/partner")

(defvar f1 (create-Triple kasper kind anneroos))
(defvar f2 (create-Triple simone kind anneroos))
(defvar f3 (create-Triple anneroos ouder kasper))
(defvar f4 (create-Triple anneroos ouder simone))
(defvar f5 (create-Triple kasper partner simone))
(defvar f6 (create-Triple simone partner kasper))
