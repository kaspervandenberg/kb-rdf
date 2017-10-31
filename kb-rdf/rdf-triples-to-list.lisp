(in-package :kb-rdf)

(defparameter *rdf-namespace* "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(defparameter *rdf-list-first* (intern (format nil "~a~a" *rdf-namespace* "first"))
  "Object is the first datum of the list; similar to `car` in common lisp.")
(defparameter *rdf-list-rest* (intern (format nil "~a~a" *rdf-namespace* "rest"))
  "Resource is the remainder of the list; similar to `cdr` in common lisp.")
(defparameter *rdf-list-end-marker* (format nil "~a~a" *rdf-namespace* "nil")
  "Indicates that the list in finished; similar to `nil` in common lisp.")
(defparameter *package-aux-namespace* "http://rdf.kaspervandenberg.net/lisp-tryout/")
(defparameter *head-node-subject* (format nil "~a~a" *package-aux-namespace* "requested-list")
  "indicates that the object is the head of the requested list.")
(defparameter *head-node-predicate* (format nil "~a~a" *package-aux-namespace* "head")
  "indicates that the object is the head of the requested list.")

(defclass rdf-list-node ()
  ((subject 
     :initarg :subj
     :documentation "Resource (often a BNode) that identifies this node (a.k.a 
                     triplie pair; a.k.a. cons)")
   (value
     :documentation "value of the '?subject *rdf-list-first*")
   (next
     :documentation "Resource (often either a BNode or `*rdf-list-end-marker*`) 
                     of the next node in the list.")
   (partial-list
     :documentation "The lisp list from this node upto the `*rdf-list-end-marker*`,
                     or `nil` if the list could not yet be constructed."))
  (:documentation "A list in RDF is constructed from triple pairs analogue to 
                  conses in lisp.  This class stores such a pair."))

   
(defmacro with-gensyms (syms &body body)
  "Generate fresh symbols, that do not colide with existing symbols, to use in 
   the macro `body`.  The macro body can refer to the generated symbol by 
   expanding the meta-symbols in `syms`."
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))


(defmacro repository (store lookup equality instantiator)
  "Repository pattern.  Store a collection of instances in the dynamic variable 
   named `store`. `lookup` will either retrieve an exiting item from `store` or 
   create a fresh item and store and return that item. 
   `equality is a function that returns `T` when the item and the queried key are 
   equal and `nil` otherwise.
   `instantiator` create a fresh instance to store for a given key."
  (with-gensyms (existing-matches fresh-instance)
    `(progn
       (defvar ,store nil
         ,(format nil "Dynamic variable that stores a collection of instances retrieved via ~a." lookup))
       (defun ,lookup (query)
         ,(format nil "Lookup an instance in the dynamic variable, ~a, or create, insert, and return a fresh instance." store)
         (let ((,existing-matches (remove-if (lambda (item) (not (funcall ,equality item query))) ,store)))
           (if (not (eql ,existing-matches nil))
             (first ,existing-matches)
             (let ((,fresh-instance (funcall ,instantiator query)))
               (setf ,store (cons ,fresh-instance ,store))
               ,fresh-instance)))))))

(repository kb-rdf::*nodes-by-subject*
            kb-rdf::get-list-node-by-subject
            (lambda (item query) (equalp (slot-value item 'subject) query))
            (lambda (subject) (make-instance 'rdf-list-node :subj subject)))


(defvar *head-node* nil
  "Dynamic variable that indicate the `rdf-list-node` that is the head of the 
   requested list.")


(defgeneric store-list-triple (node pred obj)
  (:documentation
    "Accept an RDF triple that is part of an RDF list and store it as a 
     `rdf-list-node` in `*nodes-by-subject*`."))

(if nil (progn
(defmethod store-list-triple ((node rdf-list-node) (pred (eql *rdf-list-first*)) obj)
  "Store the data value triple, analogue to `car`, of the list node pair in the 
   `rdf-list-node` that corresponds to the triple's subject."
  (setf (slot-value node 'value) obj))

(defmethod store-list-triple ((node rdf-list-node) (pred (eql *rdf-list-rest*)) obj)
  "Store the list remainder triple, analogue to `cdr`, of the list node pair in
   the `rdf-list-node` that corresponds to the triple's subject."
  (setf (slot-value node 'next) obj))
          ))

(defun add-triple (s p o)
  (let ((pred-str (puri:render-uri p nil)))
    (if (equalp pred-str *head-node-predicate*)
      (setf *head-node* (get-list-node-by-subject o))
      (store-list-triple (get-list-node-by-subject s) (intern pred-str) o))))


(defgeneric get-partial-list (node)
  (:documentation 
    "Build the partial list from this node to the end of the list.  Returns the 
     list and `T` if the list could be build; and, returns `(values nil nil)` if
     the list could not be build, e.g. when triples are missing."))
   
(defmethod get-partial-list ((node (eql nil)))
  "`(eql node nil)` indicates missing data, therefore the list can not be 
   constructed"
  (values nil nil))
  
(defmethod get-partial-list ((node puri:uri))
  "The uri points to a (named) `rdf-list-node` or it is the special value 
   `*rdf-list-end-marker*`, in the latter case this is the end of the list"
  (if (equalp (puri:render-uri next nil) *rdf-list-end-marker*)
    (values nil T)
    (get-partial-list (get-list-node-by-subject next))))

(defmethod get-partial-list ((node cl-rdfxml:blank-node))
  "A BNode is the next part of the list"
  (get-partial-list (get-list-node-by-subject node)))

(defmethod get-partial-list ((node rdf-list-node))
  "Construct a partial list from the triple pair using `value` as car and 
   `next` as cdr."
  (with-slots (value next partial-list) node
    (cond
      ((slot-boundp node 'partial-list)
       (values partial-list T))
      
      ((or (not (slot-boundp node 'value))
           (not (slot-boundp node 'next)))
       (values nil nil))
      
      (T
       (multiple-value-bind (rest-list calculated-p) (get-partial-list next)
         (if calculated-p
           (progn
             (setf partial-list (cons value rest-list))
             (values partial-list calculated-p))
           (values nil calculated-p)))))))


(defun query-list (head-node-where-clause &key prefixes (endpoint *sparql-endpoint*))
  "Retrieve the list pointed to by the head-node-where-clause and convert it into a lisp list."
  (check-type head-node-where-clause string #.(format nil "a SPARQL clause that assigns the head of the list resource to ?head; e.g.:~% * \"~a\", or~% * \"~a\".~%"
                                            "ex:mylist ex:contents ?head."
                                            "bind (ex:named-list-head as ?head)"
                                            ))
  (sparql-query #'add-triple (build-list-query head-node-where-clause) :prefixes prefixes :endpoint endpoint))


(defun build-list-query (head-node-where-clause)
  "Transform a clause that selects the list head into a query that constructs the list."
  (check-type head-node-where-clause string #.(format nil "a SPARQL clause that assigns the head of the list resource to ?head; e.g.:~% * \"~a\", or~% * \"~a\".~%"
                                            "ex:mylist ex:contents ?head."
                                            "bind (ex:named-list-head as ?head)"
                                            ))
  (format nil "~%~a~%~a~%~a~%~a~%~a~%~%~a~%~a~%~a~%~a~%~a~%~a~%"
          "construct {"
          (format nil "  <~a> <~a> ~a." *head-node-subject* *head-node-predicate* "?head")
          (format nil "  ~a <~a> ~a;" "?node" *rdf-list-first* "?value")
          (format nil "     <~a> ~a." *rdf-list-rest* "?rest")
          "}"
          "where {"
          (format nil "  ~a." head-node-where-clause)
          (format nil "  ~a <~a>* ~a." "?head" *rdf-list-rest* "?node") 
          (format nil "  ~a <~a> ~a;" "?node" *rdf-list-first* "?value")
          (format nil "     <~a> ~a." *rdf-list-rest* "?rest")
          "}"))


