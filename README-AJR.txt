	-*- mode: text -*-


* TODO:

Refactor src into "lisp-matrix", "support", etc...

* Done:

Lisp-matrix is now in own package, and supports a lisp-matrix-user
playground.

* Introduction

Lisp-Matrix is a reasonably modern, flexible system for numeric
algebra in Lisp.  

Rif, Tomas Papp, and Mark H contributed various critical pieces (code,
design, code, testing, code, benchmarks, code, and more code).

I'm just a dwarf standing on the shoulders of giants.

** Design

Basic approach:

Storage can be done at the lisp or external level.  Selection is
user-specific, since you might have various people doing one or
either, depending oupon the nature of the computations that are
required.  For exmpel large LAPACK-ish computatiosn are required to be
done externally whereas stuff which is more lisp-cerntreic culd be
done internally.

We avoid deep copying whenever possible.  current scablability of
storage is not nearly as great as the storgeage required to do
interesting things with biomedical data or economic data.  Whoops.
This means tht we spend time on strucutres where copying is optional,
done when needed to break the connectino from the initial data
storage.  

We, of course, means "them".



* Requirements:

Works with: SBCL
Others: not clear (though CLISP is targetted)

* Packages:

From Rif:

** org.middleangle.cl-blapack
** org.middleangle.foreign-numeric-vector

From Tomas:

** ffa
** array-operations

From Mark:

** lisp-matrix integration (also found in ffa and cl-blapack).

In general:

** cffi  (depends on babel, alexandria )
** cl-utilities
** iterate
** metabang-bind
** asdf-system-connections
** lift (depends on trivial-timeout)

These dependencies need to be better worked out.

* Usage

See:

TODO.lisp    : things that don't work but should
lm-demo.lisp : things that might work but should
