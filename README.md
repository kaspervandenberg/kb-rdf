# kb-rdf
Single person hoby project to learn how a tripple store is implemented by developping one.

To become a RDF-tripplestore and SPARQL engine written in Common Lisp.  I intend to store tripples
indexed in various orders (e.g. subject/predicate/object, predicate/subject/object) in an adaptation
of Prokopec's CTrie (see [Cache-Aware Lock-Free Concurrent Hash Tries][Prokopec2011] by Aleksandar
Prokopec, Phill Bagwell, and Martin Ordersky): one that stores hierarchical keys, like a non-hashing
CTrie, by the hash code of the parts, a nested hashing tries.  This in a thread-save lock-free manner
as the CTrie.

Currently working on kb-rdf/ctrie and kb-rdf/bitindexed-list.  Some of the older attempts are messy; I will
clean these.



[Prokopec]: https://infoscience.epfl.ch/record/166908/files/ctries-techreport.pdf "ctries-techreport.pdf @infoscience.epfl.ch"
