
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

(deftype MultiFn [methodName 
                  dispatchFn 
                  defaultDispatchVal
                  hierarchy
                  methodTable
                  preferTable
                  methodCache
                  cachedHierarchy]
    IMultiFn
    (reset [self]
        (doseq [m ["methodTable"
                   "methodCache"
                   "preferTable"]]
            (py/setattr self m {}))
        (py/setattr self "cachedHierarchy" nil))
    
    (addMethod [self dispatchVal method]
        (py/setattr self 
                    "methodTable" 
                    (assoc (.getMethodTable self)
                           dispatchVal 
                           method))
        (.resetCache self))
    
    (removeMethod [self dispatchVal]
        (py/setattr self 
                    "methodTable" 
                    (dissoc (.getMethodTable self)
                            dispatchVal))
        (.resetCache self))
    
    (preferMethod [self x y]
        (if (.prefers x y)
            (raise (Exception (str "Preference conflict in multimethod "
                                   methodName
                                   " "
                                   X
                                   " is already prefered to "
                                   Y))))
        (py/setattr self
                    "preferTable"
                    (assoc (.getPreferTable self)
                           X
                           (conj (get (.getPreferTable self) X #{}) Y)))
        (.resetCache self))

    (prefers [x y]
        (let [xprefs (get (.getPreferTable self) x)]
             (cond (and (not (nil? xperfs)) 
                        (contains? xperfs y))
                    true
                   (some #(.prefers self x (first %)) (parents y))
                    true
                   (some #(.prefers self (first %) y) (parents x))
                    true
                   :default
                    false)))
    (isA [x y]
        (isa? hierarchy x y))
    
    (dominates [x y]
        (or (.prefers self x y)
            (.isA self x y)))
    
    (resetCache []
        (py/setattr self "methodCache" (.getMethodTable self))
        (py/setattr self "cachedHierarchy" @hierarchy)
        methodCache)
    
    (getMethod [dispatchVal]
        (if (not (= cachedHierarchy @hierarchy))
            (.resetCache self))
        (let [targetFn (get methodCache dispatchVal)]
             (if (not (nil? targetFn))
                 targetFn
                 (let [targetFn (.findAndCacheBestMethod self dispatchVal)]
                      (if (not (nil? targetFn))
                          targetFn
                          targetFn (get (.getMethodTable)
                                        defaultDispatchVal))))))
    (getFn [dispatchVal]
        (let [targetFn (.getMethod self dispatchVal)]
             (if (nil? targetFn)
                 (throw (Exception (str "No method in multimethod " 
                                        methodName
                                        " for dispatch value "
                                        dispatchVal)))
                 targetFn)))
    
    
    (__call__ [& args]
        (apply (.getFn self (apply dispatchFn args))
               args))
                               
                   
                 
