import clojure
import rpythontest
import sys
fn = rpythontest.run.deref()


def entry_point(argv):
    fn(0)
    return 0

def target(*args):
    return entry_point, None
    
if __name__ == "__main__":
    entry_point(sys.argv)
