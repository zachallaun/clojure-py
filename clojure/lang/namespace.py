from clojure.lang.areference import AReference
from clojure.lang.atomicreference import AtomicReference
from clojure.lang.persistenthashmap import EMPTY as EMPTY_MAP
from clojure.lang.cljexceptions import (InvalidArgumentException,
                                        IllegalStateException,
                                        ArityException,
                                        IllegalArgumentException)
import clojure.lang.rt as RT
from clojure.lang.symbol import Symbol, symbol
from clojure.lang.var import Var
import clojure.standardimports as stdimps
import sys, types

namespaces = AtomicReference(EMPTY_MAP)

def areDifferentInstancesOfSameClassName(o1, o2):
    return o1.__class__ is o2.__class__

def create(name):
    """Creates a namespace with a given name and adds standard imports to it.
    
    clojure.core is special-cased: being the first created module, some
    specific Vars must be added "by hand".
    """
    mod = types.ModuleType(name)
    mod.__file__ = "<interactive namespace>"
    for i in dir(stdimps):
        if i.startswith("_"):
            continue
        setattr(mod, i, getattr(stdimps, i))
    if mod.__name__ == "clojure.core":
        setattr(mod, "*ns*", Var(mod, "*ns*", mod).setDynamic())
    return mod

def findOrCreateIn(module, parts):
    if not parts:
        return module
    part = parts[0]
    parts = parts[1:]
    if hasattr(module, part):
        return findOrCreateIn(getattr(module, part), parts)
    mod = types.ModuleType(module.__name__ + "." + part)
    setattr(module, part, mod)
    return findOrCreateIn(mod, parts)

def findOrCreate(name):
    """Returns a namespace with a given name, creating it if it doesn't exist.
    """
    if isinstance(name, Symbol):
        name = name.name
    if not sys.modules.get(name):
        sys.modules[name] = create(name)
    return sys.modules[name]

def remove(name):
    if isinstance(name, types.ModuleType):
        name = name.__name__
    if isinstance(name, Symbol):
        name = name.name
    if name not in sys.modules:
        raise KeyError("Module {} not found".format(name))
    if name == "clojure.core":
        raise IllegalArgumentException("Cannot remove clojure.core namespace")
    del sys.modules[name]
    return None

def find(name, fromns=None):
    if isinstance(name, types.ModuleType):
        return name
    if name in getattr(fromns, "__aliases__", {}):
        return fromns.__aliases__[name]
    return sys.modules.get(str(name))

def findItem(ns, sym):
    if sym.ns is not None and symbol(sym.ns) in getattr(ns, "__aliases__", {}):
        sym = symbol(ns.__aliases__[symbol(sym.ns)].__name__, sym.name)
       
    if isinstance(sym, Symbol):
        if ns is None:
            ns = sys.modules["clojure.core"] # we need this to boostrap files
        if sym.ns == ns.__name__:
            return getattr(ns, sym.name, None)
        if sym.ns is not None:
            mod = find(sym.ns, ns)
            return getattr(mod, sym.name, None)
        return getattr(ns, str(sym), None)
    return getattr(ns, sym)

def findModule(sym, module = None):
    if module is None:
        sym = sym.split(".")
        parts = sym[1:]
        name = sym[0]
        if name not in sys.modules:
            return None
        if parts:
            return findModule(parts, sys.modules[name])
        return sys.modules[name]

    name = sym[0]
    parts = sym[1:]
    if not hasattr(module, name):
        return None
    if parts:
        return findModule(parts, getattr(module, name))
    return getattr(module, name)

def intern(ns, sym):
    if isinstance(sym, str):
        sym = symbol(str)

    if sym.ns is not None:
        raise InvalidArgumentException(
            "Can't intern namespace-qualified symbol")

    ns = find(ns)
    if hasattr(ns, str(sym)):
        v = getattr(ns, str(sym))
        if not isinstance(v, Var):
            raise Exception(
                "can't redefine {0} as {1}: is not Var".format(v, sym))
        if ns.__name__ == v.ns.__name__:
            return v
    v = Var(ns, sym)
    setattr(ns, sym.name, v)
    return v

