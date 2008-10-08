	-*- mode: text -*-


* TODO:

Refactor src into "lisp-matrix", "support", etc...


* Description

Lisp-Matrix is a reasonably modern, flexible system for numeric
algebra in Lisp.  

Rif, Tomas Papp, and Mark H contributed various critical pieces (code,
design, code, testing, code, benchmarks, code, and more code).

I'm just a dwarf standing on the shoulders of giants.


* Requirements:

not firm: SBCL or CLISP.

* Packages:

From Rif:

** org.middleangle.cl-blapack
** org.middleangle.foreign-numeric-vector

From Tomas:

** ffa
** array-operations

In general:

** cffi  (depends on babel, alexandria )
** cl-utilities
** iterate
** metabang-bind
** asdf-system-connections
** lift (depends on trivial-timeout)

These dependencies need to be better worked out.

* Usage

