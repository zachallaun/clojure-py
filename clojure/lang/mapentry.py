"""
March 26, 2012 -- documented
"""

from clojure.lang.amapentry import AMapEntry

class MapEntry(AMapEntry):
    """Concrete implementation of an AMapEntry."""
    def __init__(self, key, value):
        """Instantiate a MapEntry.

        key -- any object
        value -- any object"""
        self._key = key
        self._value = value

    def getKey(self):
        """Return the key of this MapEntry."""
        return self._key

    def getValue(self):
        """Return the value of this MapEntry."""
        return self._value
