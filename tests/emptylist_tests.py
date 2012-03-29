"""emptylist_tests.py

Thursday, March 29 2012
"""

import unittest
from cStringIO import StringIO

from clojure.lang.persistentlist import EMPTY, EmptyList
from clojure.lang.cljexceptions import IllegalStateException

uobj = object()
pseudoMetaData = object()

class EmptyListTests(unittest.TestCase):
    def setUp(self):
        pass
    def test__hash___PASS(self):
        self.assertEqual(EMPTY.__hash__(), 1)
    # only basic equality tests here
    def test__eq___PASS(self):
        self.assertTrue(EMPTY.__eq__(EMPTY))
        self.assertTrue(EMPTY.__eq__(()))
        self.assertTrue(EMPTY.__eq__([]))
    def test__ne___PASS(self):
        self.assertFalse(EMPTY.__ne__(EMPTY))
        self.assertFalse(EMPTY.__ne__(()))
        self.assertFalse(EMPTY.__ne__([]))
    def test__iter___PASS(self):
        self.assertEqual(EMPTY.__iter__(), None)
    def testWithMeta_PASS(self):
        l1 = EMPTY.withMeta(pseudoMetaData)
        l2 = l1.withMeta(uobj)
        self.assertFalse(l1.meta() is l2.meta())
        # equal with different meta data
        self.assertTrue(l1.__eq__(l2))
    def testFirst_PASS(self):
        self.assertEqual(EMPTY.first(), None)
    def testNext_PASS(self):
        self.assertEqual(EMPTY.next(), None)
    def testMore_PASS(self):
        self.assertTrue(EMPTY.more() is EMPTY)
    def testCons_PASS(self):
        l = EMPTY.cons(uobj)
        self.assertFalse(EMPTY is l)
        self.assertEqual(len(l), 1)
        self.assertEqual(l.first(), uobj)
    def testEmpty_PASS(self):
        self.assertTrue(EMPTY.empty() is EMPTY)
    def testPeek_PASS(self):
        self.assertEqual(EMPTY.peek(), None)
    def testPop_FAIL(self):
        self.assertRaises(IllegalStateException, EMPTY.pop)
    def testCount_PASS(self):
        self.assertEqual(EMPTY.count(), 0)
    def testSeq_PASS(self):
        self.assertEqual(EMPTY.seq(), None)
    def testWriteAsString_PASS(self):
        sio = StringIO()
        EMPTY.writeAsString(sio)
        self.assertEqual(sio.getvalue(), "()")
    def testWriteAsReplString_PASS(self):
        sio = StringIO()
        EMPTY.writeAsReplString(sio)
        self.assertEqual(sio.getvalue(), "()")
    def test__str___PASS(self):
        self.assertEqual(str(EMPTY), "()")
    def test__repr___PASS(self):
        self.assertEqual(repr(EMPTY), "()")
    def test__len___PASS(self):
        self.assertEqual(EMPTY.__len__(), 0)
    
