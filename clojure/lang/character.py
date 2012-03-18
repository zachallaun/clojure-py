#!/usr/bin/python -t

"""character.py

<stirfoo@gmail.com>
Tuesday, March 13 2012
"""

from iprintable import IPrintable

charToSymbol = {
    " " : "space",
    "\n" : "newline",
    "\t" : "tab",
    "\f" : "formfeed",
    "\b" : "backspace",
    "\r" : "return",
    }

# XXX: unicode parent is so the compiler will handle it, for now.
# I don't know how the compiler code works O_o
class Character(unicode, IPrintable):
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
    def writeAsString(self, writer):
        writer.write("fuck" + self.c)
    def writeAsReplString(self, writer):
        writer.write("\\" + charToSymbol.get(self.c, self.c))
