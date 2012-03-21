from clojure.lang.obj import Obj
from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.lang.iseq import ISeq
from clojure.lang.sequential import Sequential
from clojure.lang.counted import Counted
from clojure.lang.ihasheq import IHashEq
from clojure.lang.iterable import Iterable
import clojure.lang.rt as RT
from clojure.lang.iprintable import IPrintable
from clojure.lang.ipersistentset import IPersistentSet



class ASeq(Obj, Sequential, ISeq, IHashEq, Iterable, IPrintable):
    def __eq__(self, other):
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
        return not self == other

    def __getitem__(self, idx):
        s = self.seq()
        c = 0
        while s is not None:
            if c == idx:
                return s.first()
            c += 1
            s = s.next()
        return None

    def seq(self):
        return self

    def count(self):
        i = 1
        for s in self.interator():
            if isinstance(s, Counted):
                return i + s.count()
            i += 1
        return i

    def more(self):
        s = self.next()
        if s is None:
            from clojure.lang.persistentlist import EMPTY
            return EMPTY
        return s

    def first(self):
        raise AbstractMethodCall(self)

    def __iter__(self):
        s = self.seq()
        while s is not None:
            yield s.first()
            s = s.next()

    def hasheq(self):
        ret = 1
        for s in self:
            ret = 31 * ret + hash(s) #Util.hasheq(s.first())#FIXME: Util is... where?
        return ret

    def writeAsString(self, writer):
        writer.write(repr(self))

    def writeAsReplString(self, writer):
        writer.write(repr(self))
        
    def cons(self, other):
        from clojure.lang.cons import Cons
        return Cons(other, self)
