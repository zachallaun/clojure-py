#!/usr/bin/python -t

"""stringseq-tests.py

Wednesday, March 21 2012
"""

import unittest
from clojure.lang.character import character, Character
from clojure.lang.stringseq import stringseq, StringSeq

class TestStringSeq(unittest.TestCase):
    def setUp(self):
        self.emptySS = stringseq("")
        self.xySS = stringseq("xy")
        self.xChar = character("x")
        self.yChar = character("y")
    def testStringSeqArray_PASS(self):
        self.assertEqual(self.emptySS, None)
        self.assertEqual(self.xySS.array, "xy")
    def testStringSeqFirstNext_PASS(self):
        self.assertEqual(self.xySS.first(), self.xChar)
        self.assertEqual(self.xySS.next().first(), self.yChar)
        self.assertEqual(self.xySS.next().next(), None)
