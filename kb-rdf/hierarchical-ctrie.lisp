(defvar *bucket-size* 64)
(defvar *bucket-bitmask* #b111111)
(defvar *bucket-bitmask-length* 6)


(defclass INode ()
  ((main :initform nil)))

(defclass CNode ()
  ((filled-nodes :initform (make-array (list *bucket-size*)
				       :element-type 'bit
				       :initial-element 0))
   (pointers :initform (make-array '(0)))
   (key-index :initform 0)
   (fragment-index :initform 0)))


(defgeneric hierarchical-key-fragment (cnode hierarchical-key)
  (:documentation "Return the key-fragment of the `hierarchical-key` to select the subnode branch of `cnode`."))

(defgeneric hash-fragment (cnode key)
  (:documentation "Return the key fragment of `key1 to select the subnode branch of `cnode`."))

(defgeneric hierarchical-key-bit (cnode hierarchical-key)
  (:documentation "Return a bitmask for `filled-nodes` that has the bit for hierarchical-key-fragment '1' and the other bits '0'."))

(defgeneric hierarchical-key-pointer-index (cnode hierarchical-key)
  (:documentation "Return the index in `pointers` of `cnode` for `hierarchical-key`."))


(defmethod hash-fragment ((CNode cnode) key)
  (logand *bucket-bitmask* (ash (sxhash key) (* -1 *bucket-bitmask-length* (slot-value cnode 'fragment-index)))))

(defmethod hierarchical-key-fragment ((CNode cnode) hierarchical-key)
  (hash-fragment cnode (nth (slot-value cnode 'key-index) hierarchical-key)))

(defmethod hierarchical-key-bit ((CNode cnode) hierarchical-key)
  (ash 1 (hierarchical-key-fragment cnode hierarchical-key)))

;; TODO Werkt niet conversie bitvector en int niet ondersteund
(defmethod hierarchical-key-pointer-index ((Cnode cnode) hierarchical-key)
  (logcount (logand (slot-value cnode 'filled-nodes) (- (hierarchical-key-bit cnode hierarchical-key) 1))))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'eqaul)))
    #'(lambda )))

;; Try out
(let ((i1 (make-instance 'INode))) (sb-ext:atomic-update (slot-value i1 'main) (lambda (_) 'test)) (print (slot-value i1 'main)))
(let ((key (list 'a 'b 'c))) key)

(defvar test-expr "http://example.org/rdf/test")
(format nil "~b~%~b~%~b"
	(sxhash test-expr)
	(hash-fragment test-expr 0)
	(hash-fragment test-expr 1))

(nth 0 '(resource "http://example.org/pred" "http://example.org/subj2"))

(logcount 6)

(- (ash 1 (hash-fragment test-expr 0)) 1)

(ash 1 63)

(reduce)

;; Reference
(subseq)
(concat)
