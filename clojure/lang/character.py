#!/usr/bin/python -t

"""character.py

<stirfoo@gmail.com>
Tuesday, March 13 2012
"""

# XXX: unicode parent is so the compiler will handle it, for now.
# I don't know how the compiler code works O_o
class Character(unicode):
    """A single character.

    Python has no character type. It's simply a string of length 1. The
    clojure-py printer fns need a way to discern a string of one character
    from a clojure-py character. If not, \\x will always be written as
    'x'. For instance on the repl, \\x should return as \\x.

    This class simply stores a string of length 1, as Python, but provides a
    distinct type for the clojure-py print routines."""
    def __init__(self, c):
        self.c = c.encode("utf-8")
    # Java Character.charValue() ... sort of
    def charValue(self):
        return self.c
    def __str__(self):
        return self.c
