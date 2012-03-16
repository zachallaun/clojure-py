from clojure.lang.apersistentvector import APersistentVector
from clojure.lang.persistentvector import create as createVector
from clojure.lang.cljexceptions import IndexOutOfBoundsException
from clojure.lang.iprintable import IPrintable


class AMapEntry(APersistentVector, IPrintable):
    def __getitem__(self, i):
        if i == 0:
            return self.getKey()
        elif i == 1:
            return self.getValue()
        else:
            raise IndexOutOfRangeException()

    def asVector(self):
        return createVector(self.getKey(), self.getValue())

    def assocN(self, i, val):
        return self.asVector().assocN(i, val)

    def __len__(self):
        return 2
        
    def __contains__(self, x):
        if x == 0 or x == 1:
            return True
        return False
        
    def seq(self):
        return self.asVector().seq()

    def cons(self, o):
        return self.asVector().cons(o)

    def empty(self):
        return None

    def pop(self):
        return PersistentVector(self.getKey())

    def writeAsString(self, writer):
        writer.write(repr(self))

    def writeAsReplString(self, writer):
        writer.write(repr(self))
