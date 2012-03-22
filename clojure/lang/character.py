#!/usr/bin/python -t

"""character.py

Tuesday, March 13 2012
"""

import weakref
from iprintable import IPrintable

charToSymbol = {
    " " : "space",
    "\n" : "newline",
    "\t" : "tab",
    "\f" : "formfeed",
    "\b" : "backspace",
    "\r" : "return",
    }

# associated core functions:
# * char?
# * char
# * char-array
# * chars

# associated core-print functions
# * char-escape-string

# TODO: maybe store the charcter internally as an integer codepoint

class Character(IPrintable):
    """A single character.

    Python has no character type. It's simply a string of length 1. The
    clojure-py printer fns need a way to discern a string of one character
    from a clojure-py character. If not, \\x will always be written as
    'x'. For instance on the repl, \\x should return as \\x.

    This class simply stores a string of length 1, as Python, but provides a
    distinct type for the clojure-py print routines."""
    def __init__(self, c):
        self._c = c.encode("utf-8")
    def __hash__(self):
        return hash(self._c)
    # quack quack
    def __int__(self):
        return ord(self._c.decode("utf-8"))
    # XXX: temporary until writeAsString and writeAsReplString come on line
    def __repr__(self):
        return "\\" + charToSymbol.get(self._c, self._c)
    def writeAsString(self, writer):
        writer.write(self._c)
    def writeAsReplString(self, writer):
        writer.write("\\" + charToSymbol.get(self._c, self._c))
    @property
    def c(self):
        """Return the internal one string character."""
        return self._c

# This is but a start to a Character implementation.
# I think this might be the overhead halgari was alluding to.
# It's not thread safe either.

# "x" => Character
_charCache = {}

def character(c):
    """Return a Character instance set to the one character Python string c.

    Character instances are cached so:
    character("x") is character("x") => True
    """
    wref = _charCache.get(c)
    if wref:
        o = wref()
        if o is not None:
            return o
    ch = Character(c)
    _charCache[c] = weakref.ref(ch)
    for k in list(_charCache):
        if _charCache[k]() is None:
            del _charCache[k]
    return ch
