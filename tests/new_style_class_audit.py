## This script will attempt to audit the enitre clojure.lang namespace and insure
## that all classes implement newstyle classes.
import clojure
import clojure.lang
import new
passed = []
failed = []

for module in dir(clojure.lang): # modules
    mod = getattr(clojure.lang, module)
    for itm in dir(mod):
        i = getattr(mod, itm)
        if isinstance(i, new.classobj):
            print module, itm, "FAILED"
            failed.append(i)
        elif isinstance(i, type):
            passed.append([module, itm, ">>>>> PASSED <<<<"])
            
print 
print len(failed), " FAILED"            
print len(passed), " PASSED"
        
