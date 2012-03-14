#!/usr/bin/env python

import sys
import os.path
import traceback

from clojure.lang.symbol import symbol
from clojure.lang.var import Var, intern as internVar
from clojure.util.byteplay import *


try:
    import readline
except ImportError:
    pass
else:
    import os
    import atexit
    histfile = os.path.join(os.path.expanduser("~"), ".clojurepyhist")
    if not os.path.isfile(histfile):
        with open(histfile, 'a'):
            os.utime(histfile, None)
        os.chmod(histfile, int('640',8))
    try:
        readline.read_history_file(histfile)
    except IOError:
        # Pass here as there isn't any history file, so one will be
        # written by atexit
        pass
    atexit.register(readline.write_history_file, histfile)

import __builtin__
import sys
import os.path

_old_import_ = __builtin__.__import__
def import_hook(name, globals=None, locals=None, fromlist=None, level = -1):
    try:
        return _old_import_(name, globals, locals, fromlist, level)
    except ImportError:
        pass

    conv = name.replace(".", "/")
    for p in ["."] + sys.path:
        f = p + "/" + conv + ".clj"
        if os.path.exists(f):
            requireClj(f)
            try:
                return _old_import_(name, globals, locals, fromlist, level)
            except ImportError:
                raise ImportError("could not find module " + name + " after loading " + f)

    raise ImportError("module " + name + " not found")

__builtin__.__import__ = import_hook

from clojure.lang.lispreader import read
from clojure.lang.fileseq import StringReader
from clojure.lang.globals import currentCompiler
import clojure.lang.rt as RT
from clojure.lang.compiler import Compiler
from clojure.lang.symbol import Symbol, symbol
import cPickle

VERSION = "0.1.0h"



def requireClj(filename, stopafter=None):
    with open(filename) as fl:
        r = StringReader(fl.read())

    RT.init()
    comp = Compiler()
    comp.setFile(filename)
    currentCompiler.set(comp)

    #o = open(filename+".cljc", "w")
    try:
        while True:
            s = read(r, False, None, True)
            if s is None:
                break
            #cPickle.dump(s, o)
            try:
                res = comp.compile(s)
                comp.executeCode(res)
                if stopafter is not None:
                    if hasattr(comp.getNS(), stopafter):
                        break
            except Exception as exp:
                print s, filename
                raise

            while True:
                ch = r.read()

                if ch == "":
                    raise IOError()

                if ch not in [" ", "\t", "\n", "\r"]:
                    r.back()
                    break
    except IOError as e:
        pass

    #o.close()

#requireClj(os.path.dirname(__file__) + "/core.clj")
import clojure.core

def main():
    RT.init()
    comp = Compiler()
    currentCompiler.set(comp)
    comp.setNS(symbol("user"))
    last3 = [None, None, None]

    def execute(string):
        r = StringReader(string)
        s = read(r, False, None, True)
        res = comp.compile(s)
        return comp.executeCode(res)

    if not sys.argv[1:]:
        while True:
            for i, value in enumerate(last3, 1):
                sym = symbol('*%s' % i)
                comp.pushName(sym.name)
                code = []
                v = internVar(comp.getNS(), sym)
                v.setDynamic(True)
                code.append((LOAD_CONST, v))
                code.append((LOAD_ATTR, "bindRoot"))
                if isinstance(value, Var):
                    code.append((LOAD_CONST, value.deref()))
                else:
                    code.extend(comp.compile(value))
                code.append((CALL_FUNCTION, 1))
                v.setMeta(sym.meta())
                comp.popName()
                comp.executeCode(code)

            try:
                line = raw_input(comp.getNS().__name__ + "=> ")
            except EOFError:
                break

            if not line:
                continue

            while unbalanced(line):
                try:
                    line += raw_input('.' * len(comp.getNS().__name__) + '.. ')
                except EOFError:
                    break

            # Propogate break from above loop.
            if unbalanced(line):
                break

            try:
                out = execute(line)
            except Exception:
                traceback.print_exc()
            else:
                last3.pop()
                last3.insert(0, out)
                print out
    else:
        for x in sys.argv[1:]:
            requireClj(x)


def unbalanced(s):
    return (s.count('(') != s.count(')')
            or s.count('[') != s.count(']')
            or s.count('{') != s.count('}'))


if __name__ == "__main__":
    main()
