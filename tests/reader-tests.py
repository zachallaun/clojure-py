#!/usr/bin/python -t
# -*- coding: utf-8 -*-

"""reader-tests.py

Friday, March 16 2012
"""

import re
import string
import unittest

from random import choice
from fractions import Fraction
from clojure.lang.lispreader import read, readDelimitedList
from clojure.lang.symbol import Symbol
from clojure.lang.character import character, Character
from clojure.lang.ipersistentlist import IPersistentList
from clojure.lang.persistentlist import PersistentList
from clojure.lang.persistentlist import EmptyList
from clojure.lang.persistentvector import PersistentVector
from clojure.lang.persistenthashmap import PersistentHashMap
from clojure.lang.persistenthashset import PersistentHashSet
from clojure.lang.fileseq import StringReader
from clojure.lang.cljexceptions import ReaderException

regexType = type(re.compile(""))
nilType = type(None)
trueType = type(True)
falseType= type(False)

class TestReader(unittest.TestCase):
    # literal integers
    def testIntegerReader_PASS(self):
        # base 8
        for k, v in base8IntegerMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
        # base 10
        for k, v in base10IntegerMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
        # base 16
        for k, v in base16IntegerMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
        # base N
        for k, v in baseNIntegerMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
    def testIntegerReader_FAIL(self):
        for t in integer_FAIL:
            r = StringReader(t)
            self.assertRaises(ReaderException, read, r, False, None, False)
    # literal floating point
    def testFloatingPointReader_PASS(self):
        for k, v in floatingPointMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
    def testFloatingPointReader_FAIL(self):
        for t in floatingPoint_FAIL:
            r = StringReader(t)
            self.assertRaises(ReaderException, read, r, False, None, False)
    # literal ratios 
    def testRationalReader_PASS(self):
        for k, v in rationalMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
    def testRationalReader_FAIL(self):
        for t in rational_FAIL:
            r = StringReader(t)
            self.assertRaises(ReaderException, read, r, False, None, False)
    # literal characters
    def testCharacterReader_PASS(self):
        for k, v in literalCharacterMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False), v)
    def testCharacterReader_FAIL(self):
        for s in literalCharacter_FAIL:
            r = StringReader(s)
            self.assertRaises(ReaderException, read, r, False, None, False)
    # literal strings
    def testStringReader_PASS(self):
        for k, v in literalStringMap_PASS.items():
            r = StringReader('"' + k + '"')
            self.assertEqual(read(r, False, None, False), v)
    def testStringReader_FAIL(self):
        # special case, missing trailing "
        r = StringReader('"foo')
        self.assertRaises(ReaderException, read, r, False, None, False)
        for s in literalString_FAIL:
            r = StringReader('"' + s + '"')
            self.assertRaises(ReaderException, read, r, False, None, False)
    # literal regex pattern strings
    def testRegexPattern_PASS(self):
        for k, v in regexPatternMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False).pattern, v.pattern)
    def testRegexPattern_FAIL(self):
        for s in regexPattern_FAIL:
            r = StringReader(s)
            self.assertRaises(ReaderException, read, r, False, None, False)
    # literal raw regex pattern strings
    def testRawRegexPattern_PASS(self):
        for k, v in rawRegexPatternMap_PASS.items():
            r = StringReader(k)
            self.assertEqual(read(r, False, None, False).pattern, v.pattern)
    def testRawRegexPattern_FAIL(self):
        for s in rawRegexPattern_FAIL:
            r = StringReader(s)
            self.assertRaises(ReaderException, read, r, False, None, False)
    # delimited lists
    def testDelimitedLists_PASS(self):
        # length test
        for k, v in delimitedListLength_PASS.items():
            r = StringReader(k)
            delim = k[-1]
            self.assertEqual(readDelimitedList(delim, r, False), v)
    # returned type tests
    def testReturnedType_PASS(self):
        for k, v in returnedType_PASS.items():
            r = StringReader(k)
            self.assertEqual(type(read(r, False, None, False)), v)
    # miscellaneous failures
    def testMiscellaneous_FAIL(self):
        for s in miscellaneous_FAIL:
            r = StringReader(s)
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
    >>> pprint(eval(gen_baseNIntegerMap_PASS()))"""
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
    # regex was fubar, didn't allow zeros after the first digit
    "100/203" : Fraction(100, 203),
    "-100/203" : Fraction(-100, 203),
    "+100/203" : Fraction(100, 203),
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
    "\\x" : character("x"),
    "\\ " : character(" "),
    "\\X" : character("X"),
    # newline after the \
    """\\
""" : character("\n"),
    # named characters
    "\\space" : character(" "),
    "\\newline" : character("\n"),
    "\\return" : character("\r"),
    "\\backspace" : character("\b"),
    "\\formfeed" : character("\f"),
    "\\tab" : character("\t"),
    # octal
    "\\o0" : character("\x00"),
    "\\o41" : character("!"),
    "\\o377" : character(u"\u00ff"),
    # hex
    "\\u03bb" : character(u"\u03bb"),
    # BZZZZT!
    # Because this file is encoded as UTF-8, and the reader is expecting ASCII,
    # it will crap out every time. 
    # "\\λ" : character(u"\u03bb"),
    }

literalCharacter_FAIL = [
    # According to a random web page:
    # The only reason the range D800:DFFF is invalid is because of UTF-16's
    # inability to encode it.
    "\ud800", "\udfff",
    # missing char at eof
    "\\",
    # not enough digits after \u (\u is the character u)
    "\u1", "\u22", "\u333",
    # too many digits after \u
    "\u03bbb",
    # too many digits after \o
    "\o0333",
    # octal value > 0377
    "\o400"
    ]

# ======================================================================
# Literal String Cases
# These are tests that conform to Clojure. Some Python string syntax is
# not permitted:
# \U, \N{foo}, \x, \v, \a
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
    "\u03bb": u"\u03bb",
    "\u03bb@": u"\u03bb@",
    "@\u03bb": u"@\u03bb",
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
    "\\o041"
    # not enough digits after \u
    "\\u", "\\u3", "\\u33", "\\u333",
    "@\\u", "@\\u3", "@\\u33", "@\\u333",
    "\\u@", "\\u3@", "\\u33@", "\\u333@",
    # octal value > 0377
    "\\400", "@\\400", "\\400@",
    ]

# ======================================================================
# Regular Expression Pattern
#
# Each key is the string sent to lispreader. The escapes have to be
# handled in such a way as to allow the reader to do escape
# interpretation. If Python would treat the escape special, it needs
# an additional \ before sending it to the reader.
# ======================================================================

regexPatternMap_PASS = {
    # all using #"", not raw #r""
    '#""' : re.compile(""),
    '#"."' : re.compile("."),
    '#"^."' : re.compile("^."),
    '#".$"' : re.compile(".$"),
    '#".*"' : re.compile(".*"),
    '#".+"' : re.compile(".+"),
    '#".?"' : re.compile(".?"),
    '#".*?"' : re.compile(".*?"),
    '#".+?"' : re.compile(".+?"),
    '#".??"' : re.compile(".??"),
    '#".{3}"' : re.compile(".{3}"),
    '#".{3,}"' : re.compile(".{3,}"),
    '#".{,3}"' : re.compile(".{,3}"),
    '#".{3,3}"' : re.compile(".{3,3}"),
    '#".{3,3}"' : re.compile(".{3,3}"),
    '#".{3,3}?"' : re.compile(".{3,3}?"),
    # None of these \ are special. Python will send them to the reader as is.
    # \ . \ ^ \ $, etc.
    '#"\.\^\$\*\+\?\{\}\[\]"' : re.compile("\.\^\$\*\+\?\{\}\[\]"),
    '#"[a-z]"' : re.compile("[a-z]"),
    '#"[]]"' : re.compile("[]]"),
    '#"[-]"' : re.compile("[-]"),
    # Nor are these
    '#"[\-\]\[]"' : re.compile(r"[\-\]\[]"),
    # or these
    '#"[\w\S]"' : re.compile("[\w\S]"),
    '#"[^5]"' : re.compile("[^5]"),
    # or the |
    '#"A|B[|]\|"' : re.compile("A|B[|]\|"),
    # or ( )
    '#"([()]\(\))"' : re.compile("([()]\(\))"),
    '#"(?iLmsux)"' : re.compile("(?iLmsux)"),
    '#"(?iLmsux)"' : re.compile("(?iLmsux)"),
    '#"(:?)"' : re.compile("(:?)"),
    '#"(?P<foo>)"' : re.compile("(?P<foo>)"),
    '#"(?P<foo>)(?P=foo)"' : re.compile("(?P<foo>)(?P=foo)"),
    '#"(?# comment )"' : re.compile("(?# comment )"),
    '#"(?=foo)"' : re.compile("(?=foo)"),
    '#"(?!foo)"' : re.compile("(?!foo)"),
    '#"(?<=foo)bar"' : re.compile("(?<=foo)bar"),
    '#"(?<!foo)bar"' : re.compile("(?<!foo)bar"),
    '#"(?P<foo>)(?(foo)yes|no)"' : re.compile("(?P<foo>)(?(foo)yes|no)"),
    #       |  |<---- Python will send two \'s to the lisp reader, not four
    '#"(.+) \\\\1"' : re.compile("(.+) \\1"),
    '#"(.+) \\\\1"' : re.compile(r"(.+) \1"),
    # send one \ each, so the octal sequences are interpreted in lispreader
    # >>> u"\377" == "\377"   # funky warning on the Python repl
    '#"\\377\\021"' : re.compile(u"\377\021"),
    # Again, send one \ each. Python would interpret \1 as the char 0x01
    # *before* sending it to lispreader.
    '#"[\\1\\2\\3\\4\\5\\6\\7\\10]"' : re.compile("[\1\2\3\4\5\6\7\10]"),
    # Python does not interpret \A, but it does \b
    # The dict value here is a raw string so the char sequence will be:
    # \ A \ \ b \ B, etc.
    '#"\A\\\\b\B\d\D\s\S\w\W\Z"' : re.compile(r"\A\b\B\d\D\s\S\w\W\Z"),
    # dict val is a raw string, and Python interprets all these chars
    '#"\\\\a\\\\b\\\\f\\\\n\\\\r\\\\t\\\\v"' : re.compile(r"\a\b\f\n\r\t\v"),
    # I want Python to interpret here. lispreader will simply return
    # 0x07, 0x08 etc. (no escape interpretation)
    '#"\a\b\f\n\r\t\v"' : re.compile("\a\b\f\n\r\t\v"),
    # Send \ and letter separately. lispreader will see \ n and
    # return 0x0a (reader interpretation)
    '#"\\a\\b\\f\\n\\r\\t\\v"' : re.compile("\a\b\f\n\r\t\v"),
    # \N, \u, and \U are only special in a unicode string (in Python)
    '#"\N{DIGIT ZERO}{5, 10}"' : re.compile(u"\N{DIGIT ZERO}{5, 10}"),
    '#"\u03bb{1,3}"' : re.compile(u"\u03bb{1,3}"),
    '#"\U000003bb{1,3}"' : re.compile(u"\U000003bb{1,3}"),
    # but \x is always special, hence the \\
    '#"\\xff\\x7f"' : re.compile(u"\xff\x7f"),
    
'''#"(?x)
     # foo
     [a-z]
     # bar
     [0-9a-zA-Z_]+
     "''' : re.compile("""(?x)
     # foo
     [a-z]
     # bar
     [0-9a-zA-Z_]+
     """),
    }

regexPattern_FAIL = [
    # # unmatched paren, bracket, (can't make it catch a missing } O_o)
    '#"([()]\(\)"', '#"["',
    # foo not defined
    '#"(?(foo)yes|no)"',
    # bogus escape 
    '#"[\\8]"',
    # need 4 hex digits
    '#"\u"', '#"\u1"', '#"\u12"', '#"\u123"',
    # need 8 hex digits
    '#"\U"', '#"\U1"', '#"\U12"', '#"\U123"', '#"\U1234"', '#"\U12345"',
    '#"\U123456"', '#"\U1234567"',
    # need 2 hex digits
    '#"\\x"', '#"\\x1"',
    # missing }, missing ",  can't escape }
    '#"\N{foo"', '#"\N{foo', '#"\N{foo\\}}"',
    # unknown name
    '#"\N{KLINGON LETTER NG}"',
    # empty {}
    '#"\N{}"', '#"\N{   }"',
    ]

rawRegexPatternMap_PASS = {
    '#r""' : re.compile(r""),
    '#r"\\."' : re.compile(r"\."),
    '#r"\\."' : re.compile(r"\."),
    '#r"\\n"' : re.compile(r"\n"),
    '#r"\.\^\$\*\+\?\{\}\[\]"' : re.compile(r"\.\^\$\*\+\?\{\}\[\]"),
    '#r"[\-\]\[]"' : re.compile(r"[\-\]\[]"),
    '#r"[\w\S]"' : re.compile(r"[\w\S]"),
    '#r"A|B[|]\|"' : re.compile(r"A|B[|]\|"),
    '#r"([()]\(\))"' : re.compile(r"([()]\(\))"),
    '#r"(.+) \\1"' : re.compile(r"(.+) \1"),
    '#r"\\377\\021"' : re.compile(ur"\377\021"),
    '#r"[\\1\\2\\3\\4\\5\\6\\7\\10]"' : re.compile(r"[\1\2\3\4\5\6\7\10]"),
    '#r"\A\\b\B\d\D\s\S\w\W\Z"' : re.compile(r"\A\b\B\d\D\s\S\w\W\Z"),
    '#r"\\a\\b\\f\\n\\r\\t\\v"' : re.compile(r"\a\b\f\n\r\t\v"),
    '#r"\a\b\f\n\r\t\v"' : re.compile("\a\b\f\n\r\t\v"),
    '#r"\N{DIGIT ZERO}{5, 10}"' : re.compile(ur"\N{DIGIT ZERO}{5, 10}"),
    '#r"\u03bb{1,3}"' : re.compile(ur"\u03bb{1,3}"),
    '#r"\\\u03bb{1,3}"' : re.compile(ur"\\u03bb{1,3}"),
    '#r"\\\\\u03bb{1,3}"' : re.compile(ur"\\\u03bb{1,3}"),
    '#r"\\\\\\\u03bb{1,3}"' : re.compile(ur"\\\\u03bb{1,3}"),
    '#r"\U000003bb{1,3}"' : re.compile(ur"\U000003bb{1,3}"),
    '#r"\\xff\\x7f"' : re.compile(ur"\xff\x7f"),
    '#r"\\0"' : re.compile(ur"\0"),
    '#r"\\01"' : re.compile(ur"\01"),
    '#r"\\012"' : re.compile(ur"\012"),
    '''#r"\\
"''' : re.compile(r"""\
"""),
    }

rawRegexPattern_FAIL = [
    # craps out the regex compiler
    '#r"\\x"',
    # can't end with an odd number of \
    '#r"\\"',                   # #r"\"    ; in clojure-py
    '#r"\\\\\\"',               # #r"\\\"  ; in clojure-py
    # missing trailing "
    '#r"foo',
    # need 4 hex digits
    '#r"\u"', '#r"\u1"', '#r"\u12"', '#r"\u123"',
    # need 8 hex digits
    '#r"\U"', '#r"\U1"', '#r"\U12"', '#r"\U123"', '#r"\U1234"', '#r"\U12345"',
    '#r"\U123456"', '#r"\U1234567"',
    ]

# ======================================================================
# Literal Delimited Lists
# ======================================================================

# The keys define the clojure syntax of any object that would result in a call
# to lispreader.readDelimitedList() (minus the leading macro character(s)).
# Some objects like map and set have the same terminating character `}'. So
# there is only one entry for both.
#
# The value is a the expected contents of the Python list returned from
# readDelimitedList(). Integers are used because I don't care what type the
# items are. There are separate tests for that.
delimitedListLength_PASS = {
    "]" : [],
    "}" : [],
    ")" : [],
    "0]" : [0],
    "0)" : [0],
    "0}" : [0],
    "0 0]" : [0, 0],
    "0 0)" : [0, 0],
    "0 0}" : [0, 0],
    }

# ======================================================================
# Returned Type
# ======================================================================
returnedType_PASS = {
    "" : nilType,
    "," : nilType,
    " " : nilType,
    """
""" : nilType,
    "\r" : nilType,
    "\n" : nilType,
    "\r\n" : nilType,
    "\n\r" : nilType,
    "\t" : nilType,
    "\b" : nilType,
    "\f" : nilType,
    ", \n\r\n\t\n\b\r\f" : nilType,
    "\v" : Symbol,              # O_o
    "\\x" : Character,
    "%foo" : Symbol,            # not in an anonymous function #()
    "[]" : PersistentVector,
    "()" : EmptyList,
    "{}" : PersistentHashMap,
    '"foo"' : str,              # TODO: always return unicode, never str
    '#"foo"' : regexType,
    '#r"foo"' : regexType,
    "#()" : PersistentList,
    "#{}" : PersistentHashSet,
    "'foo" : PersistentList,
    "~foo" : PersistentList,
    "~@(foo)" : PersistentList,
    "#^:foo()" : EmptyList,
    "^:foo()" : EmptyList,
    "; comment" : nilType,
    "#_ foo" : nilType,
    "0" : int,
    "0x0" : int,
    "041" : int,
    "2r10" : int,
    "2.2" : float,
    "2e-3" : float,
    "1/2" : Fraction,
    "foo" : Symbol,
    ".3" : Symbol,
    "+.3" : Symbol,
    "-.3" : Symbol,
    "true" : trueType,
    "True" : Symbol,
    "false" : falseType,
    "False" : Symbol,
    "nil" : nilType,
    "None" : Symbol,
    }

# ======================================================================
# Miscellaneous Failures
# Any type of random failures should go here
# ======================================================================

miscellaneous_FAIL = [
    # always raises
    "#<unreadable object>",
    # deref not implemented (yet)
    "@foo",
    # reader eval not implemented (yet)
    "#=foo",
    ]
