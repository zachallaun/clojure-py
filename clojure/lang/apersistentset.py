from clojure.lang.ifn import IFn
from clojure.lang.cljexceptions import AbstractMethodCall, ArityException
from clojure.lang.ipersistentset import IPersistentSet
from clojure.lang.apersistentmap import createKeySeq
import clojure.lang.rt as RT
from clojure.lang.iprintable import IPrintable

class APersistentSet(IPersistentSet, IFn, IPrintable):
    def __init__(self, impl):
        self.impl = impl
        self._hash = -1

    def __getitem__(self, item):
        return self.impl[item]

    def __contains__(self, item):
        return item in self.impl

    def __len__(self):
        return len(self.impl)

    def seq(self):
        return createKeySeq(self.impl.seq())

    def __call__(self, *args):
        if len(args) != 1:
            raise ArityException()
        return self.impl[args[0]]

    def __eq__(self, other):
        if self is other:
            return True

        if not isinstance(other, IPersistentSet):
            return False

        for s in self.impl:
            if s not in other or not other[s] == self[s]:
                return False
        return True

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        if self._hash == -1:
            hsh = 0
            s = self.seq()
            while s is not None:
                e = s.first()
                hsh += hash(e)
                s = s.next()
            self._hash = hsh
        return self._hash

    def writeAsString(self, writer):
        writer.write("#{")
        s = self.seq()
        while s is not None:
            RT.protocols.writeAsString(s.first(), writer)
            if s.next() is not None:
                writer.write(" ")
            s = s.next()
        writer.write("}")

    def writeAsReplString(self, writer):
        writer.write("#{")
        s = self.seq()
        while s is not None:
            RT.protocols.writeAsReplString(s.first(), writer)
            if s.next() is not None:
                writer.write(" ")
            s = s.next()
        writer.write("}")
