#!/usr/bin/python -t

"""stringseq.py

Wednesday, March 21 2012
"""

from clojure.lang.character import character
from clojure.lang.indexableseq import IndexableSeq

class StringSeq(IndexableSeq):
    """A sequence of Character instances."""
    def first(self):
        """Return the first Character."""
        return character(self.array[self.i])
    def next(self):
        """Return the StringSeq after the first Character."""
        if self.i + 1 < len(self.array):
            return StringSeq(self.array, self.i + 1)
        return None
    # temporary kaka until I grok the print protocol
    def __repr__(self):
        return "[{0}]".format(" ".join(map(str,
                                           map(character,
                                               self.array[self.i:]))))
        
def stringseq(s):
    """Return a StringSeq

    s -- str or unicode instance"""
    if len(s) == 0:
        return None
    return StringSeq(s, 0)
