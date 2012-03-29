"""subvec-test.py

Wednesday, March 28 2012
"""

import unittest

import clojure.lang.persistentvector as pv
from clojure.lang.apersistentvector import SubVec
from clojure.lang.persistentvector import PersistentVector
from clojure.lang.cljexceptions import IndexOutOfBoundsException

pseudoMetaData = object()

class TestSubVec(unittest.TestCase):
    def setUp(self):
        # [0 1 2 3 4 5 6 7 8 9]
        #         [4 5 6]
        self.parent = pv.vec(range(10))
        self.sv = SubVec(pseudoMetaData, self.parent, 4, 7)
        self.oneItemSv = SubVec(None, self.parent, 0, 1)
    # nth()
    def testNth_PASS(self):
        self.assertEqual(self.sv.nth(0), 4)
        self.assertEqual(self.sv.nth(1), 5)
        self.assertEqual(self.sv.nth(2), 6)
    def testNth_FAIL(self):
        # below lower bound
        # These are accepted in Clojure, but I think it's a bug.
        self.assertRaises(IndexOutOfBoundsException, self.sv.nth, -4)
        self.assertRaises(IndexOutOfBoundsException, self.sv.nth, -3)
        self.assertRaises(IndexOutOfBoundsException, self.sv.nth, -2)
        self.assertRaises(IndexOutOfBoundsException, self.sv.nth, -1)
        # beyond upper bound
        self.assertRaises(IndexOutOfBoundsException, self.sv.nth, 3)
    # __len__()
    def test__len___PASS(self):
        self.assertEqual(len(self.sv), 3)
    # empty()
    def testEmpty_PASS(self):
        v = self.sv.empty()
        self.assertEqual(v.meta(), self.sv.meta())
    # pop()
    def testtPop_PASS(self):
        emptyV = self.oneItemSv.pop()
        self.assertTrue(isinstance(emptyV, PersistentVector))
        self.assertEqual(len(emptyV), 0)
        oneLessSv = self.sv.pop()
        self.assertTrue(isinstance(oneLessSv, SubVec))
        self.assertEqual(len(oneLessSv), 2)
        
    # No pop fail test because pop returns an empty PersistentVector.
    # There is no empty SubVec.
        
    # meta()
    def testMeta_PASS(self):
        self.assertEqual(pseudoMetaData, self.sv.meta())
    # withMeta()
    def testWithMeta_PASS(self):
        # return self
        outsv = self.sv.withMeta(pseudoMetaData)
        print id(outsv), id(self.sv)
        self.assertTrue(outsv is self.sv)
        # return new SubVec
        meta = object()
        outsv = self.sv.withMeta(meta)
        self.assertTrue(meta, outsv.meta())
