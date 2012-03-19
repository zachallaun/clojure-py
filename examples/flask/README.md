Flask example
=============

A quick test of pyclojure using python's premiere web micro framework.

`util.clj` contains utility macros and functions. App managment is
rather primative, as STM primatives are needed to properly manage
mutable data over multiple threads.

`main.clj` contains a simple flask application, defined using macros
in `util.clj`.

To run
------

From shell:

    clojurepy main.clj

This does assume you have flask installed. If you don't, simply run
`pip install flask` (or better, install it from your distribution's
repositories).
