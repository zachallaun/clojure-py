from clojure.lang.cljexceptions import AbstractMethodCall


class IPrintable(object):
    def writeAsString(self, writer):
        raise AbstractMethodCall(self)
    def writeAsReplString(self, writer):
        raise AbstractMethodCall(self)        
