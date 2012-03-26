"""
March 25, 2012 -- documented
"""

import cStringIO

import clojure.lang.rt as RT
from clojure.lang.atomicreference import AtomicReference
from clojure.lang.apersistentvector import APersistentVector
from clojure.lang.cljexceptions import (ArityException,
                                        IndexOutOfBoundsException,
                                        IllegalStateException)


class PersistentVector(APersistentVector):
    def __init__(self, *args):
        """Instantiate a PersistentVector

        cnt -- integer
        shift -- integer
        tail -- list
        root -- Node, holds a list of size 32
        _meta -- IPersistentHashMap, meta data"""
        if len(args) == 4:
            cnt, shift, root, tail = args
            _meta = None
        elif len(args) == 5:
            _meta, cnt, shift, root, tail = args
        else:
            raise ArityException()
        self._meta = _meta
        self.cnt = cnt
        self.shift = shift
        self.root = root
        self.tail = tail

    def __call__(self, idx):
        """Return the item at idx.

        idx -- integer >= 0

        May raise IndexOutOfBoundsException."""
        return self.nth(idx)

    def tailoff(self):
        if self.cnt < 32:
            return 0
        return ((self.cnt - 1) >> 5) << 5

    def arrayFor(self, i):
        if 0 <= i < self.cnt:
            if i >= self.tailoff():
                return self.tail
            node = self.root
            for level in range(self.shift, 0, -5):
                node = node.array[(i >> level) & 0x01f]
            return node.array
        raise IndexOutOfBoundsException()

    def nth(self, i, notFound=None):
        """Return the item at index i or notFound."""
        if 0 <= i < self.cnt:
            node = self.arrayFor(i)
            return node[i & 0x01f]
        return notFound

    def meta(self):
        """Return this vector's meta data as an IPersistentHashMap."""
        return self._meta

    def assocN(self, i, val):
        if 0 <= i < self.cnt:
            if i >= self.tailoff():
                newTail = self.tail[:]
                newTail[i & 0x01f] = val

                return PersistentVector(self.meta(), self.cnt, self.shift,
                                        self.root, newTail)

            n = doAssoc(self.shift, self.root, i, val)
            return PersistentVector(self.meta(), self.cnt, self.shift, n,
                                    self.tail)
        if i == self.cnt:
            return self.cons(val)

        raise IndexOutOfBoundsException()

    def __len__(self):
        """Return the number of items in this vector."""
        return self.cnt

    def withMeta(self, meta):
        """Return a PersistentVector.

        meta -- an IPersistentMap

        The returned vector will contain this vectors contents and have meta
        attached."""
        return PersistentVector(meta, self.cnt, self.shift, self.root,
                                self.tail)

    def cons(self, val):
        if self.cnt - self.tailoff() < 32:
            newTail = self.tail[:]
            newTail.append(val)
            return PersistentVector(self.meta(), self.cnt + 1, self.shift,
                                    self.root, newTail)

        tailnode = Node(self.root.edit, self.tail)
        newshift = self.shift
        if (self.cnt >> 5) > (1 << self.shift):
            newroot = Node(self.root.edit)
            newroot.array[0] = self.root
            newroot.array[1] = newPath(self.root.edit, self.shift, tailnode)
            newshift += 5
        else:
            newroot = self.pushTail(self.shift, self.root, tailnode)

        return PersistentVector(self.meta(), self.cnt + 1, newshift, newroot,
                                [val])

    def pushTail(self, level, parent, tailnode):
        subidx = ((self.cnt - 1) >> level) & 0x01f
        ret = Node(parent.edit, parent.array[:])

        if level == 5:
            nodeToInsert = tailnode
        else:
            child = parent.array[subidx]
            nodeToInsert = (self.pushTail(level - 5, child, tailnode)
                            if child is not None
                            else newPath(self.root.edit, level - 5, tailnode))
        ret.array[subidx] = nodeToInsert
        return ret

    def empty(self):
        """Return an empty PersistentVector."""
        return EMPTY.withMeta(self.meta())

    def pop(self):
        if not self.cnt:
            raise IllegalStateException("Can't pop empty vector")
        if self.cnt == 1:
            return EMPTY.withMeta(self.meta())
        if self.cnt - self.tailoff() > 1:
            newTail = self.tail[:]
            newTail.pop()
            return PersistentVector(self.meta(), self.cnt - 1, self.shift,
                                    self.root, newTail)

        newtail = self.arrayFor(self.cnt - 2)

        newroot = self.popTail(self.shift, self.root)
        newshift = self.shift
        if newroot is None:
            newroot = EMPTY_NODE
        if self.shift > 5 and newroot.array[1] is None:
            newroot = newroot.array[0]
            newshift -= 5
        if newroot is None:
            pass
        return PersistentVector(self.meta(), self.cnt - 1, newshift, newroot,
                                newtail)

    def popTail(self, level, node):
        subidx = ((self.cnt - 2) >> level) & 0x01f
        if level > 5:
            newchild = self.popTail(level - 5, node.array[subidx])
            if newchild is None and not subidx:
                return None
            else:
                ret = Node(self.root.edit, node.array[:])
                ret.array[subidx] = newchild
                return ret
        elif not subidx:
            return None
        else:
            ret = Node(self.root.edit, node.array[:])
            ret.array[subidx] = None
            return ret
        
    def __str__(self):
        """Return a string representation of this vector.

        The vector will be formatted as a Python list.
        """
        s = []
        for x in self:
            s.append(str(x))
        return "[" + ", ".join(s) + "]"

    def __repr__(self):
        """Return a string representation of this vector.

        A persistent vector has no Python readable representation. The
        *semantic* validity of the vector is unknown."""
        sio = cStringIO.StringIO()
        self.writeAsReplString(sio)
        return "<{0}.{1} at 0x{2:x} {3}>".format(self.__module__,
                                                 type(self).__name__,
                                                 id(self),
                                                 sio.getvalue())

#    def __eq__(self, other):
#        if other is self:
#            return True
#        if not hasattr(other, "__len__"):
#            return False
#        if not hasattr(other, "__getitem__"):
#            return False
#
#        if not len(self) == len(other):
#            return False
#
#        for x in range(len(self)):
#            if self[x] != other[x]:
#                return False
#
#        return True

# ======================================================================
# Node
# ======================================================================

class Node(object):
    def __init__(self, edit, array=None):
        self.edit = edit
        self.array = array if array is not None else [None] * 32


def newPath(edit, level, node):
    if not level:
        return node
    ret = Node(edit)
    ret.array[0] = newPath(edit, level - 5, node)
    return ret


def doAssoc(level, node, i, val):
    ret = Node(node.edit, node.array[:])
    if not level:
        ret.array[i & 0x01f] = val
    else:
        subidx = (i >> level) & 0x01f
        ret.array[subidx] = doAssoc(level - 5, node.array[subidx], i, val)
    return ret

# ======================================================================
# Creation
# ======================================================================

def vec(seq):
    if isinstance(seq, APersistentVector):
        return seq
    s = RT.seq(seq)
    v = EMPTY
    while s is not None:
        v = v.cons(RT.first(s))
        s = RT.next(s)
    return v

def create(*args):
    x = EMPTY
    for z in args:
        x = x.cons(z)
    return x

# ======================================================================
# Pseudo-Singletons
# ======================================================================

NOEDIT = AtomicReference()
EMPTY_NODE = Node(NOEDIT)
EMPTY = PersistentVector(0, 5, EMPTY_NODE, [])
