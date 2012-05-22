from clojure.lang.cljexceptions import AbstractMethodCall
from clojure.util.treadle import treadle as tr

MAX_IFN_ARITY = 20

def _init():
    invokes = {}
    
    def make_arg(x):
        return tr.Argument("arg"+str(x))
    
    for x in range(MAX_IFN_ARITY):
        slf = tr.Argument("self")
        args = [slf] + map(make_arg, range(x))
        fn = tr.Func(args, tr.Raise(tr.Call(tr.Const(AbstractMethodCall), slf)))
        invokes["invoke"+str(x)] = fn.toFunc()
        
    globals()["IFn"] = type("IFn", (object,), invokes)
                    
_init()
        