import sys
import os.path

sys.path = [os.path.dirname(__file__)+"../"] + sys.path
import clojure
import unittest
from clojure.lang.var import Var
from clojure.lang.cljkeyword import keyword

from clojure.main import requireClj

import clojure.core


def mapTest(ns, var):
    class Test(unittest.TestCase):
        def testVar(self):
            var()
    
    name = ns + str(var)
    tst = Test
    tst.__name__ = name
    globals()[name] = tst
            

for x in os.listdir(os.path.dirname(__file__)):
    if x.endswith(".clj") and x.find("test") >= 0:
        print "Reading tests from",  x
        requireClj(os.path.join(os.path.dirname(__file__),x))
        folder, file = os.path.split(x)
        ns, ext = os.path.splitext(x)
        module = sys.modules["tests."+ns]
        
        for idx in dir(module):
            var = getattr(module, idx)
            if isinstance(var, Var) and str(var).endswith("tests"):
                meta = var.meta()
                if meta is not None and meta[keyword("test")]:
                    mapTest(ns, var) 
                    

