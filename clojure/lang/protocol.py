from clojure.lang.namespace import find as findNamespace





def getFuncName(protocol, funcname):
    return str(protocol) + funcname
    
class ProtocolFn(object):
    """Defines a function that dispatches on the type of the first argument
    passed to __call__"""
    
    def __init__(self, fname):
        self.dispatchTable = {}
        self.name = intern(fname)
        self.attrname = intern("__proto__" + self.name)
        
    def extend(self, tp, fn):
        try:
            setattr(tp, self.attrname, fn)
        except:
            self.dispatchTable[tp] = fn
            
    def __call__(self, *args):
        x = type(args[0])
        if hasattr(x, self.attrname):
            return getattr(x, self.attrname)(*args)
        else:
            return self.dispatchTable[x](*args)
        
    
    
    
