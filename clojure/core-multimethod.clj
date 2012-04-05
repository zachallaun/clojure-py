
(definterface IInitable
    (_init [self]))

(definterface IMultiFn
    (reset [self])
    (addMethod [self dispatchVal method])
    (removeMethod [self dispatchVal])
    (preferMethod [self dispatchValX dispatchValY])
    (prefers [self x y])
    (isA [self x y])
    (resetCache [self])
    (getMethod [self dispatchVal])
    (getFn [self dispatchVal])
    (findAndCacheBestMethod [self dispatchVal])
    (__call__ [& args])
    (getMethodTable [self])
    (getPreferTable [self]))


