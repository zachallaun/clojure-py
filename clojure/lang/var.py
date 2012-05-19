import contextlib
import types

from clojure.lang.iref import IRef
from clojure.lang.ifn import IFn
from clojure.lang.settable import Settable
from clojure.lang.aref import ARef
from clojure.lang.cljexceptions import (ArityException,
                                        InvalidArgumentException,
                                        IllegalStateException)
from clojure.lang.persistenthashmap import EMPTY
from clojure.lang.threadutil import ThreadLocal, currentThread
from clojure.lang.symbol import Symbol
from clojure.lang.cljkeyword import Keyword
from clojure.lang.iprintable import IPrintable
from clojure.lang.atomicreference import AtomicReference
import persistentarraymap

privateKey = Keyword("private")
macrokey = Keyword("macro")
STATIC_KEY = Keyword("static")
dvals = ThreadLocal()
privateMeta = persistentarraymap.create([privateKey, True])
UNKNOWN = Symbol("UNKNOWN")


def pushThreadBindings(bindings):
    f = dvals.get(lambda: Frame())
    bmap = f.bindings
    for v in bindings:
        value = bindings[v]
        if not v.dynamic:
            raise IllegalStateException(
                "Can't dynamically bind non-dynamic var: {0}/{1}".
                format(v.ns, v.sym))
        v.validate(v.getValidator(), value)
        v.threadBound = True
        bmap = bmap.assoc(v, TBox(currentThread(), value))
    dvals.set(Frame(bmap, f))


def popThreadBindings():
    f = dvals.get(lambda: Frame())
    if f.prev is None:
        raise IllegalStateException("Pop without matching push")
    dvals.set(f.prev)


@contextlib.contextmanager
def threadBindings(bindings):
    pushThreadBindings(bindings)
    try:
        yield
    finally:
        popThreadBindings()


class Var(ARef, Settable, IFn, IPrintable):
    def __init__(self, *args):
        """Var initializer

        Valid calls:
        - Var(namespace, symbol, root)
        - Var(namespace, symbol) -- unbound Var
        - Var(root) -- anonymous Var
        - Var() -- anonymous, unbound Var
        """
        self.ns = args[0] if len(args) >= 2 else None
        self.sym = args[1] if len(args) >= 2 else None
        root = args[-1] if len(args) % 2 else UNKNOWN
        self.root = AtomicReference(root if root != UNKNOWN else Unbound(self))
        self.threadBound = False
        self._meta = EMPTY
        self.dynamic = False
        self.public = True

    def setDynamic(self, val=True):
        self.dynamic = val
        return self

    def isDynamic(self):
        return self.dynamic
        
    def setPublic(self, public = True):
        self.public = public
        
    def isPublic(self):
        return self.public
        
    def isBound(self):
        return self.getThreadBinding() is not None \
                or not isinstance(self.root.get(), Unbound)

    def set(self, val):
        self.validate(self.getValidator(), val)
        b = self.getThreadBinding()
        if b is not None:
            if currentThread() != b.thread:
                raise IllegalStateException(
                    "Can't set!: {0} from non-binding thread".format(self.sym))
            b.val = val
            return self

        raise IllegalStateException(
            "Can't change/establish root binding of: {0} with set".
            format(self.sym))
        
    def alterRoot(self, fn, args):
        return self.root.mutate(lambda old: fn(old, *(args if args else ())))

    def hasRoot(self):
        return not isinstance(self.root.get(), Unbound)

    def bindRoot(self, root):
        self.validate(self.getValidator(), root)
        oldroot = self.root.get()
        self.root.set(root)
        return self

    def __call__(self, *args, **kw):
        """Exists for Python interop, don't use in clojure code"""
        return self.deref()(*args, **kw)

    def deref(self):
        b = self.getThreadBinding()
        if b is not None:
            return b.val
        return self.root.get()

    def getThreadBinding(self):
        if self.threadBound:
            e = dvals.get(lambda: Frame()).bindings.entryAt(self)
            if e is not None:
                return e.getValue()
        return None

    def setMeta(self, meta):
        self._meta = meta
        if self._meta and self._meta[STATIC_KEY]:
            self.setDynamic(False)
        return self

    def setMacro(self):
        self.alterMeta(lambda x, y, z: x.assoc(y, z), macrokey, True)

    def writeAsString(self, writer):
        writer.write(repr(self))

    def writeAsReplString(self, writer):
        writer.write(repr(self))

    def __repr__(self):
        if self.ns is not None:
            return "#'{0}/{1}".format(self.ns.__name__, self.sym)
        return "#<Var: {0}>".format(self.sym or "--unnamed--")


def getThreadBindingFrame():
    f = Val.dvals.get(lambda: Frame())#FIXME: Val undefined
    return f


def cloneThreadBindingFrame():
    f = Val.dvals.get(lambda: Frame()).clone()#FIXME: Val undefined
    return f


def resetThreadBindingFrame(val):
    Var.dvals.set(val)


def internWithRoot(ns, sym, root, replaceRoot=True):
    from namespace import intern as namespaceIntern
    dvout = namespaceIntern(ns, sym)
    if not dvout.hasRoot() or replaceRoot:
        dvout.bindRoot(root)
    return dvout


def find(sym):
    from clojure.lang.namespace import find as findNamespace
    if sym.ns is None:
        raise InvalidArgumentException("Symbol must be namespace-qualified")
    ns = findNamespace(Symbol(sym.ns))
    if ns is None:
        raise InvalidArgumentException("No such namespace {0}".format(sym.ns))
    return getattr(ns, sym.name)


def intern(ns, name):
    from namespace import findOrCreate, intern as nsintern
    
    if isinstance(ns, types.ModuleType):
        return nsintern(ns, name)
    ns = findOrCreate(Symbol(ns))
    return nsintern(ns, name)
    
def define(ns, name, root):
    v = intern(ns, name)
    v.bindRoot(root)
    return v


def internPrivate(nsName, sym):
    ns = Namespace.findOrCreate(Symbol(nsName))#FIXME: undefined Namespace
    ret = intern(ns, Symbol(sym))
    ret.setMeta(Var.privateMeta)
    return ret


class TBox(object):
    def __init__(self, thread, val):
        self.thread = thread
        self.val = val


class Unbound(IFn):
    def __init__(self, v):
        self.v = v

    def __repr__(self):
        return "Unbound {0}".format(self.v)

    def __call__(self, *args, **kwargs):
        raise ArityException(
            "Attempting to call unbound fn: {0}".format(self.v))


class Frame(object):
    def __init__(self, bindings=EMPTY, prev=None):
        self.bindings = bindings
        self.prev = prev

    def clone(self):
        return Frame(self.bindings)
