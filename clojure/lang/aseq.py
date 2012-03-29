"""
March 25, 2012 -- documented
"""

import clojure.lang.rt as RT
from clojure.lang.obj import Obj
from clojure.lang.iseq import ISeq
from clojure.lang.counted import Counted
from clojure.lang.ihasheq import IHashEq
from clojure.lang.iterable import Iterable
from clojure.lang.iprintable import IPrintable
from clojure.lang.sequential import Sequential
from clojure.lang.ipersistentset import IPersistentSet
from clojure.lang.cljexceptions import AbstractMethodCall


class ASeq(Obj, Sequential, ISeq, IHashEq, Iterable, IPrintable):
    """An ordered collection.

    By ordered I mean the items will stay in the order in which they were
    added to the sequence.

    Accessing/Adding
    ----------------
    See: iseq.py
    __getitem__ -- s[i]
    __iter__ -- for x in s

    Meta Data
    ---------
    See: obj.py

    Equality
    --------
    __eq__, __ne__
    Can be compared with another ASeq, Sequential, tuple, or list. The
    sequences are compared recursively. Each item has its own test for
    equality. An ASeq will never be equal to a set or map, but may be equal to
    a seq on a set or map.

    Printed Representation
    ----------------------
    There are 4 printed representations. All print the contents of the
    sequence surrounded by ().
    * __repr__ -- readable by the Python reader
    * __str__ -- as Python print would display it
    * writeAsString -- clojure-py print and println, no "", \\x as x, etc.
    * writeAsReplString -- readable by the clojure-py reader
    """
    def __eq__(self, other):
        """Return True if this sequence is = to other, False otherwise."""
        if self is other:
            return True
        if not RT.isSeqable(other) or (isinstance(other,IPersistentSet)):
            return False
        se = RT.seq(other)
        if isinstance(se, RT.NotSeq):
            print other, type(other)
            return False
        ms = self.seq()
        while se is not None:
            if ms is None or not se.first() == ms.first():
                return False
            ms = ms.next()
            se = se.next()
        return ms is None

    def __ne__(self, other):
        """Return not self.__eq__(other)"""
        return not self == other

    def __getitem__(self, idx):
        """Return the item at idx or None if idx >= length self."""
        s = self.seq()
        c = 0
        while s is not None:
            if c == idx:
                return s.first()
            c += 1
            s = s.next()
        return None

    def seq(self):
        """Return this sequence (self)."""
        return self

    # ???: is this ever called
    def count(self):
        """Return the number of items in this sequence."""
        i = 1
        for s in self.interator():
            if isinstance(s, Counted):
                return i + s.count()
            i += 1
        return i

    def more(self):
        """Return an ISeq.

        If this sequence has one item in it, return an EmptyList, else return
        the tail of this sequence"""
        s = self.next()
        if s is None:
            from clojure.lang.persistentlist import EMPTY
            return EMPTY
        return s

    # def first(self):
    #     """Raise AbstractMethodCall."""
    #     raise AbstractMethodCall(self)

    def __iter__(self):
        """Return an iterator on this sequence."""
        s = self.seq()
        while s is not None:
            yield s.first()
            s = s.next()

    def hasheq(self):
        """Return the hash of this sequence."""
        ret = 1
        for s in self:
            ret = 31 * ret + hash(s)
        return ret

    def cons(self, other):
        """Return a Cons.

        other -- any object

        The Cons will have object as the head, and this sequence as the
        tail."""
        from clojure.lang.cons import Cons
        return Cons(other, self)

    def __repr__(self):
        """Return a string representation of this sequence.

        The string will be readable by the Python interpreter as a tuple. If
        this seq is empty the string "()" will be returned. The *semantic*
        validity of the list is unknown."""
        s = []
        for x in self:
            s.append(repr(x))
        return "(" + ", ".join(s) + ")"

    def writeAsString(self, writer):
        """Write (...) to writer.

        writer -- a write-able object

        Where ... is a single space delimited list of the objects in this
        sequence."""
        writer.write("(")
        s = self
        while s is not None:
            RT.protocols.writeAsString(s.first(), writer)
            if s.next() is not None:
                writer.write(" ")
            s = s.next()
        writer.write(")")

    def writeAsReplString(self, writer):
        """Write (...) to writer.

        writer -- a write-able object

        Where ... is a single space delimited list of the objects in this
        sequence. The string may be read by the clojure-py reader."""
        writer.write("(")
        s = self
        while s is not None:
            RT.protocols.writeAsReplString(s.first(), writer)
            if s.next() is not None:
                writer.write(" ")
            s = s.next()
        writer.write(")")
