from random import randint, shuffle
import unittest

from clojure.lang.persistenttreemap import PersistentTreeMap


class PersistentTreeMapTests(unittest.TestCase):
    def testScaling(self):
        m = PersistentTreeMap()
        ints = range(1000)
        shuffle(ints)
        for i in ints:
            m = m.assoc(i, randint(1, 10))
        self.assertEqual(m.count(), 1000)

    def testAddRemove(self):
        m = PersistentTreeMap()
        ints = range(100)
        shuffle(ints)
        for i in ints:
            m = m.assoc(i, randint(1, 10))
        for i in ints[:10]:
            m = m.without(i)
        self.assertEqual(m.count(), 90)
