#!/usr/bin/env python

import sys
import os.path
import traceback

from clojure.lang.symbol import symbol
from clojure.lang.var import Var, intern as internVar
from clojure.util.byteplay import *

# Create a PEP 302 import hook
class MetaImporter(object):
    def find_module(self, fullname, path=None):
        import os.path, sys
        lastname = fullname.rsplit('.', 1)[-1]
        for d in (path or (["."] + sys.path)):
            clj = os.path.join(d, lastname + '.clj')
            if os.path.exists(clj):
                self.path = clj
                return self
        return None

    def load_module(self, name):
        requireClj(self.path)
        return sys.modules[name]

sys.meta_path = [MetaImporter()]

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
                v = internVar(comp.getNS(), sym)
                v.setDynamic(True)
                if isinstance(value, Var):
                    v.bindRoot(value.deref())
                    v.setMeta(value.meta())
                else:
                    v.bindRoot(value)

            try:
                line = raw_input(comp.getNS().__name__ + "=> ")
            except EOFError:
                break

            if not line:
                continue

            while unbalanced(line):
                try:
                    new_line = '\n' + raw_input('.' * len(comp.getNS().__name__) + '.. ')
                except EOFError:
                    break

                if not new_line.strip().startswith(';'):
                    line += new_line

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
            if x.endswith('.clj'):
                requireClj(x)


def unbalanced(s):
    return (s.count('(') != s.count(')')
            or s.count('[') != s.count(']')
            or s.count('{') != s.count('}'))


if __name__ == "__main__":
    main()
