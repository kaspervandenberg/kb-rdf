;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; vim: set filetype=lisp:

(defpackage #:kb-rdf-asd
  (:use :cl :asdf))

(in-package #:kb-rdf-asd)

(defsystem kb-rdf
  :name "kb-rdf"
  :version 0.0.0
  :maintainer "Kasper van den Berg <kasper@kaspervandenberg.net>"
  :author "Kasper van den Berg <kasper@kaspervandenberg.net>"
  :description "RDF & SPARQL querying"
  :serial T
  :components ((:file "package")
               (:file "sparql-query"))
  :depends-on ("drakma"
               "cl-rdfxml"
               "puri"))


