#!/usr/bin/python -t
# -*- coding: utf-8 -*-

"""reader-tests.py

<stirfoo@gmail.com>
Friday, March 16 2012
"""

import unittest, string
from random import choice
from fractions import Fraction

from clojure.lang.lispreader import read
from clojure.lang.character import Character
from clojure.lang.fileseq import StringReader
from clojure.lang.cljexceptions import ReaderException

class TestReader(unittest.TestCase):
    def testIntegerReader_PASS(self):
        # base 8
        for k,v in base8IntegerMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
        # base 10
        for k,v in base10IntegerMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
        # base 16
        for k,v in base16IntegerMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
        # base N
        for k,v in baseNIntegerMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
    def testIntegerReader_FAIL(self):
        for t in integer_FAIL:
            r = StringReader(t)
            self.assertRaises(ReaderException, read, r, False, None, False)
    def testFloatingPointReader_FAIL(self):
        for t in floatingPoint_FAIL:
            r = StringReader(t)
            self.assertRaises(ReaderException, read, r, False, None, False)
    def testRationalReader_FAIL(self):
        for t in rational_FAIL:
            r = StringReader(t)
            self.assertRaises(ReaderException, read, r, False, None, False)
    def testFloatingPointReader_PASS(self):
        for k,v in floatingPointMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
    def testRationalReader_PASS(self):
        for k,v in rationalMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
    def testCharacterReader_PASS(self):
        for k,v in literalCharacterMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
    def testCharacterReader_FAIL(self):
        for s in literalCharacter_FAIL:
            r = StringReader(s)
            self.assertRaises(ReaderException, read, r, False, None, False)
    def testStringReader_PASS(self):
        for k,v in literalStringMap_PASS.items():
            r = StringReader('"' + k + '"')
            self.assertEqual(read(r, False, None, False), v)
    def testStringReader_FAIL(self):
        # special case, missing trailing "
        r = StringReader('"foo')
        self.assertRaises(ReaderException, read, r, False, None, False)
        for s in literalString_FAIL:
            r = StringReader('"' + s + '"')
            self.assertRaises(ReaderException, read, r, False, None, False)

# ======================================================================
# Literal Integer Cases
# ======================================================================

base8IntegerMap_PASS = {
    "00": 0, "-00": 0, "+00": 0,
    "012345670": 2739128, "-012345670": -2739128, "+012345670": 2739128,
    "06235436235462365452777171623500712635712365712236" :
        140667142011619517350321483099394425046406302L,
    "-06235436235462365452777171623500712635712365712236" :
        -140667142011619517350321483099394425046406302L,
    "+06235436235462365452777171623500712635712365712236" :
        140667142011619517350321483099394425046406302L,
    }

base10IntegerMap_PASS = {
    "0" : 0, "-0" : 0, "+0" : 0,
    "1" : 1, "-1" : -1, "+1" : 1,
    "1234567890" : 1234567890,
    "-1234567890" : -1234567890,
    "+1234567890" : 1234567890,
    "20399572305720357120320399572305720357203" :
        20399572305720357120320399572305720357203L,
    "-20399572305720357120320399572305720357203" :
        -20399572305720357120320399572305720357203L,
    "+20399572305720357120320399572305720357203" :
        20399572305720357120320399572305720357203L,
    }

base16IntegerMap_PASS = {
    "0x0" : 0, "-0x0" : 0, "+0x0" : 0,
    "0X0" : 0, "-0X0" : 0, "+0X0" : 0,
    "0x1234567890abcdefABCDEF" :
        22007822917795467892608495L,
    "-0X1234567890abcdefABCDEF" :
        -22007822917795467892608495L,
    "+0x1234567890abcdefABCDEF" :
        +22007822917795467892608495L,
    }

def gen_baseNIntegerMap_PASS():
    """Return a dict as a string to test the base-N syntax (2r101010)

    This map is eval'd below.

    Each entry is of the form:
        "2r10" : 2

    To see wtf is going on...
    >>> pprint gen_baseNIntegerMap_PASS()"""
    # don't change the order of these
    digits = "1023456789aBcDeFgHiJkLmNoPqRsTuVwXyZ"
    entries = []
    for radix in range(2, 37):
        strDigits = digits[:radix]
        res1 = int(strDigits, radix)
        res2 = int('-' + strDigits, radix)
        entry = '"%s":%d, "%s":%d, "%s":%d' \
            % ("%d%s%s" % (radix, choice('rR'), strDigits), res1,
               "-%d%s%s" % (radix, choice('rR'), strDigits), res2,
               "+%d%s%s" % (radix, choice('rR'), strDigits), res1)
        entries.append(entry)
    return "{%s}" % ",".join(entries)

baseNIntegerMap_PASS = eval(gen_baseNIntegerMap_PASS())

integer_FAIL = [
    # no f suffix
    "3333f", "-3333f", "+3333f",
    # Clojure M not a suffix (yet)
    "3333M", "-3333M", "+3333M",
    # 8 not an octal digit
    "08", "-08", "+08",
    # g not a hex digit
    "0xfgaa00", "-0xfgaa00", "+0xfgaa00",
    # z not a base 32 number
    "32rzzz", "-32rzzz", "+32rzzz",
    # radix out of range [2, 36]
     "1r0", "-1r0", "+1r0", "37r0", "-37r0", "+37r0",
    ]

# ======================================================================
# Literal Floating Point Cases
# ======================================================================

floatingPointMap_PASS = {
    # no decimal, exponent
    "0e0" : 0.0, "-0e0" : 0.0, "+0e0" : 0.0,
    "0e-0" : 0.0, "-0e-0" : 0.0, "+0e-0" : 0.0,
    "0E-0" : 0.0, "-0E-0" : 0.0, "+0E-0" : 0.0,
    "0e+0" : 0.0, "-0e+0" : 0.0, "+0e+0" : 0.0,
    "0E+0" : 0.0, "-0E+0" : 0.0, "+0E+0" : 0.0,
    # with decimal, no digit after decimal, exponent
    "0." : 0.0, "-0." : 0.0, "+0." : 0.0,
    "0.e0" : 0.0, "-0.e0" : 0.0, "+0.e0" : 0.0,
    "0.E0" : 0.0, "-0.E0" : 0.0, "+0.E0" : 0.0,
    "0.e-0" : 0.0, "-0.e-0" : 0.0, "+0.e-0" : 0.0,
    "0.E-0" : 0.0, "-0.E-0" : 0.0, "+0.E-0" : 0.0,
    "0.e+0" : 0.0, "-0.e+0" : 0.0, "+0.e+0" : 0.0,
    "0.E+0" : 0.0, "-0.E+0" : 0.0, "+0.E+0" : 0.0,
    # with decimal, digit after decimal, exponent
    "0.0" : 0.0, "-0.0" : 0.0, "+0.0" : 0.0,
    "0.0e0" : 0.0, "-0.0e0" : 0.0, "+0.0e0" : 0.0,
    "0.0E0" : 0.0, "-0.0E0" : 0.0, "+0.0E0" : 0.0,
    "0.0e-0" : 0.0, "-0.0e-0" : 0.0, "+0.0e-0" : 0.0,
    "0.0E-0" : 0.0, "-0.0E-0" : 0.0, "+0.0E-0" : 0.0,
    "0.0e+0" : 0.0, "-0.0e+0" : 0.0, "+0.0e+0" : 0.0,
    "0.0E+0" : 0.0, "-0.0E+0" : 0.0, "+0.0E+0" : 0.0,
    }

floatingPoint_FAIL = [
    # no suffix
    "3.3f", "-3.3f", "+3.3f",
    # s, f, d, l, etc. not an exponent specifier
    "23.0s-4", "-23.0f-4", "+23.0d-4",
    # double decimal
    "3..", "-3..", "+3..",
    ]

# ======================================================================
# Literal Rational Cases
# ======================================================================

rationalMap_PASS = {
    "22/7" : Fraction(22, 7),
    "-22/7" : Fraction(-22, 7),
    "+22/7" : Fraction(22, 7),
    "0/1" : Fraction(0, 1),
    "-0/1" : Fraction(0, 1),
    "+0/1" : Fraction(0, 1),
    }

rational_FAIL = [
    # These actually pass in Clojure, but are interpreted as base 10 integers,
    # not base 8.
    "033/029", "-033/029", "+033/029", 
    ]

# ======================================================================
# Literal Character Cases
# ======================================================================

literalCharacterMap_PASS = {
    # basic
    "\\x" : Character("x"),
    "\\ " : Character(" "),
    "\\X" : Character("X"),
    # newline after the \
    """\\
""" : Character("\n"),
    # named characters
    "\\space" : Character(" "),
    "\\newline" : Character("\n"),
    "\\return" : Character("\r"),
    "\\backspace" : Character("\b"),
    "\\formfeed" : Character("\f"),
    "\\tab" : Character("\t"),
    # octal
    "\\o0" : Character("\x00"),
    "\\o41" : Character("!"),
    "\\o377" : Character(u"\u00ff"),
    # hex
    "\\u03bb" : Character(u"\u03bb"),
    # BZZZZT!
    # Becuase this file is encoded as UTF-8, and the reader is expecting ASCII,
    # it will crap out every time. 
    # "\\λ" : Character(u"\u03bb"),
    }

literalCharacter_FAIL = [
    # According to a random web page:
    # The only reason the range D800:DFFF is invalid is because of UTF-16's
    # inability to encode it.
    "\\ud800", "\\udfff",
    # missing char at eof
    "\\",
    # not enough digits after \u (\u is the character u)
    "\\u1", "\\u22", "\\u333",
    # too many digits after \u
    "\\u03bbb",
    # too many digits after \o
    "\\o0333",
    # octal value > 0377
    "\\o400"
    ]

# ======================================================================
# Literal String Cases
# ======================================================================
            
literalStringMap_PASS = {
    # basic
    "": "",
    "x": "x",
    "foo": "foo",
    "0123456789": "0123456789",
    "~!@#$%^&*()_+-=[]{}';:/?>.<,": "~!@#$%^&*()_+-=[]{}';:/?>.<,",
    "qwertyuiopasdfghjklzxcvbnm": "qwertyuiopasdfghjklzxcvbnm",
    "QWERTYUIOPASDFGHJKLZXCVBNM": "QWERTYUIOPASDFGHJKLZXCVBNM",
    # escape           |  |<------ trailing escaped escape
    '\\"\\n\\t\\f\\b\\r\\\\': '"\n\t\f\b\r\\',
    # 4 hex digit
    "\\u03bb": u"\u03bb",
    "\\u03bb@": u"\u03bb@",
    "@\\u03bb": u"@\u03bb",
    # octal
    "\\0": "\x00",
    "\\0@": "\x00@",
    "@\\0": "@\x00",
    "\\41": "!",
    "\\41@": "!@",
    "@\\41": "@!",
    "\\176": "~",
    "\\176@": "~@",
    "@\\176": "@~",
    }

literalString_FAIL = [
    # invalid escape characters
    "\\x", "\\a", "\\v", "@\\x", "@\\a", "@\\v", "\\x@", "\\a@", "\\v@",
    # not enough digits after \u
    "\\u", "\\u3", "\\u33", "\\u333",
    "@\\u", "@\\u3", "@\\u33", "@\\u333",
    "\\u@", "\\u3@", "\\u33@", "\\u333@",
    # missing octal digits
    "\\o", "@\\o", "\\o@",
    # octal value > 0377
    "\\o400", "@\\o400", "\\o400@",
    # octal digits > 7
    "\\o8", "@\\o8", "\\o8@",
    ]
