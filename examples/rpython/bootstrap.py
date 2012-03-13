import clojure
import rpythontest
import sys
fn = rpythontest.run.deref()


def entry_point(argv):
    print fn(0)
    return 2

def target(*args):
    return entry_point, None
    
if __name__ == "__main__":
    entry_point(sys.argv)
