#!/usr/bin/env python
"""Main entry point to clojure-py: sets import hook, import clojure.core and
define main().
"""

import cPickle
import imp
import os.path
import sys

from clojure.lang.compiler import Compiler
from clojure.lang.fileseq import StringReader
from clojure.lang.globals import currentCompiler
from clojure.lang.lispreader import read
import clojure.lang.rt as RT
from clojure.lang.var import threadBindings


VERSION = "0.2.4"


class MetaImporter(object):
    """A PEP302 import hook for clj files.
    """

    def find_module(self, fullname, path=None):
        """Finds a clj file if there is no package with the same name.
        """
        lastname = fullname.rsplit('.', 1)[-1]
        for d in path or sys.path:
            clj = os.path.join(d, lastname + ".clj")
            pkg = os.path.join(d, lastname, "__init__.py")
            pkgc = getattr(imp, "cache_from_source",
                           lambda path: path + "c")(pkg)
            if (os.path.exists(clj) and
                not (os.path.exists(pkg) or os.path.exists(pkgc))):
                self.path = clj
                return self
        return None

    def load_module(self, name):
        """Loads a clj file, returns the corresponding namespace if it exists.
        
        If the file did not create the corresponding namespace, ImportError is
        raised.
        """
        if name not in sys.modules:
            sys.modules[name] = None # avoids circular imports
            try:
                requireClj(self.path)
            except:
                del sys.modules[name]
                raise ImportError
            sys.modules[name].__loader__ = self
        if sys.modules[name] == None:
            del sys.modules[name]
            raise ImportError
        return sys.modules[name]


sys.meta_path.append(MetaImporter())


def requireClj(filename, stopafter=None):
    """Compiles and executes the code in a clj file.
    
    If `stopafter` is given, then stop execution as soon as the `stopafter`
    name is defined in the current namespace of the compiler.
    """
    with open(filename) as fl:
        r = StringReader(fl.read())

    RT.init()
    comp = Compiler()
    comp.setFile(filename)

    with threadBindings({currentCompiler: comp}): #, open(filename + ".cljc", "w") as o:
        try:
            while True:
                EOF = object()
                s = read(r, False, EOF, True)
                if s is EOF:
                    break
                #cPickle.dump(s, o)
                try:
                    res = comp.compile(s)
                    comp.executeCode(res)
                    if stopafter is not None and hasattr(comp.getNS(), stopafter):
                        break
                except Exception as exp:
                    print s, filename
                    raise
        except IOError as e:
            pass


import clojure.core


def main():
    """Runs clj files given in sys.argv or starts a REPL if none was given.
    """
    RT.init()
    comp = Compiler()

    with threadBindings({currentCompiler: comp}):
        if not sys.argv[1:]:
            import clojure.repl
            clojure.repl.enable_readline()
            clojure.repl.run_repl(comp)
        else:
            for x in sys.argv[1:]:
                if x.endswith('.clj'):
                    requireClj(x)


if __name__ == "__main__":
    main()
