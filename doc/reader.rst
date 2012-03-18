###############################
The Lisp Reader (lispreader.py)
###############################

The purpose of the reader is to parse a stream of text, one character at a
time, and return the next clojure-py obj found.

At the time of this writing the clojure-py reader will attempt to conform
exactly to the Clojure reader. Each definition below has a subsection
*Clojure Conformance* that details any differences.

Also, this document is not the final word. It's written to document the reader
as it progresses.

**********
Whitespace
**********

The following characters are considered whitespace by the reader.

Space, Newline, Carriage Return, Tab, Backspace, Formfeed, and the Comma

Whitespace (including the comma) is used to delimit objs in the input
stream. It is then discarded. The exceptions include:

* Literal Strings. All whitespace is retained.
* Regular Expression Patterns. All whitespace is retained in the pattern
  string but ignored by the regex compiler.
  (Note: might leave this up to any re flags present in the string)
* Literal Characters. A single character following the backslash may be a
  whitespace character. It will be the character returned by the reader.

Clojure Conformance
===================

The same whitespace characters are used. See: Literal Strings, Regular
Expression Patterns, and Literal Characters for any further deviations
pertaining to whitespace.

******************
Special Characters
******************

These are defined in the two dicts:

   * lispreader.macros
   * lispreader.dispatchMacros

\\
===

Read a literal character.

Return a Python string of length one. The character following the \\ can be
specified in one of the following ways.

* A single character::

   \x, \Q, \G, \\, \_, \}, \(, \λ, \,

* A symbol

  The following symbols will be evaluated to their respective characters.
  
  * ``\space`` => ``" "``

  * ``\newline`` => ``"\n"``

  * ``\return`` => ``"\r"``

  * ``\backspace`` => ``"\b"``

  * ``\formfeed`` => ``"\f"``

  * ``\tab`` => ``"\t"``
   
* By hexidecimal unicode codepoint

  \\u followed by exactly 4 hexidecimal digits::

     \u03bb

* By octal unicode codepoint

  \\o followed by one to three octal digits.  The value must be <=
  0377 octal (255 decimal).::

     user=> \o40
     \space
     user=>

Clojure Conformance
-------------------

Same syntax.

%
===

Read:

* an argument in an anonymous function (See: #())
* a symbol (See: Symbols)

Clojure Conformance
-------------------

Same syntax.
   
\` (backquote)
==============

Syntax-quote the next obj.

Clojure Conformance
-------------------

Same syntax.


[ ]
===

Read a vector.

Zero or more objs may appear between the brackets.

Clojure Conformance
-------------------

Same syntax.
   
( )
===

Read a list.

Zero or more objs may appear between the parentheses.

Clojure Conformance
-------------------

Same syntax.
   
{ }
===

Read a map.

Zero or an even number of objs may appear between the braces. The objs
alternate, key, val, key, val ...

Clojure Conformance
-------------------

Same syntax.
   
" "
===

Read a literal string.

" followed by zero or more characters or escape sequences followed by "

The string may span several lines. The newlines will be retained in the
string::

   user=> "
   foo
   "
   "\nfoo\n"
   user=>

Escape Sequences
----------------
   A single \\ followed by
   
   * n => newline (0x0a)

   * t => horizontal tab (0x09)

   * b => backspace (0x08)

   * f => form feed (0x0c)
   
   * \\ => \\

   * r => carriage return (0x0d)

   * u

     Exactly 4 hexidecimal digits must follow. A unicode character will be
     returned.

   * o

     One to three octal digits must follow. The value must be <= 0377 octal
     (255) decimal. A unicode character will be returned.

Clojure Conformance
-------------------

Same syntax.

   
#" " and #r" "
==============

Read a regular expression pattern.

#r" " is used for raw pattern strings. (See: Python re module documentation)

Python re pattern syntax is used. Also, the string is treated as if it were
encosed in Python triple quotes """ """. This will allow the pattern to be
broken over multiple lines. If this is the case the resulting pattern object
will behave in one of two ways:
   
   1. (?x) is present in the string
   
      Whitespace and Python comments will be ignored when matching with the
      pattern object::

         user=> #"(?x)
         # comment
         [a-z]
         # comment
         [0-9a-z]
         "
         #"
         [a-z]
         [0-9a-z]
         "
	 user=>
      
   2. (?x) is not present

      Whitespace and comments will not be ignored::

         user=> #"
         # comment
         [a-z]
         # comment
         [0-9a-z]
         "
	 #"\n# comment[a-z]\n# comment[0-9a-z]\n"

How the repl prints the resulting pattern object reflects whether it was
compiled with the (?x) flag or not. 
   
A pattern object will be returned.

It would be great if we could subclass this to better define just what a
pattern object is. In Java there is: java.util.regex.Pattern. In Python a
match object is some obscure internal data structure that, AFAIK, you can't
inherit from.

Clojure Conformance
-------------------

None. The differences in Python and Java re syntax are far too many.

#( )
====

Read an anonymous function.

Special positional arguments are allowed within this syntax. %, %1, %2, %3,
\... %N, and %&. % and %1 will evaluate to the first argument passed to the
function. %2, %3, ... %N to the second, third, nth argument. %& will be a
sequence of the remaining arguments, or nil, if none.::

   user=> (def vfn #(vector %1 %2 %&))
   #'user/vfn
   user=> (vfn 1 2 3 4 5 6)
   [1 2 (3 4 5 6)]
   user=> (vfn 1 2)
   [1 2 nil]
   user=>
   
Clojure Conformance
-------------------

Same syntax.

#{ }
====

Read a set.

Zero or more objs may appear between the braces.
   
Clojure Conformance
-------------------

Same syntax.

#<
===

Throw an unconditional exception (unreadable obj follows).

Clojure Conformance
-------------------

Same syntax.

#'
===

Shorthand for the special form **var**.

Return the list ::

  (var next-obj)

Clojure Conformance
-------------------

Same syntax.

'
===

Shorthand for the special form **quote**.

return the list::

   (quote next-obj)
   
Clojure Conformance
-------------------

Same syntax.

~
===

Shorthand for the special form **unquote**.


return the list::

   (unquote next-obj)
   
Clojure Conformance
-------------------

Same syntax.

~@
===

Shorthand for the special form **unquote-splicing**.

return the list::

   (unquote-splicing next-obj)
   
Clojure Conformance
-------------------

Same syntax.

@
===

Shorthand for the function **deref**.

return the list::

   (deref next-obj)
   
Clojure Conformance
-------------------

Same syntax.

^
===

Read the next obj as meta data.

The obj following the ^ must be a *bound* symbol, keyword, string, or map. The
reader then consumes one more obj and attaches the meta data to that obj. This
last obj is returned.

1. read ^
2. read obj1
3. read obj2, attach obj1 as meta data
4. return obj2

Examples attaching meta data to a vector, then passing the vector to the
function **meta**, which returns that attached meta data. The examples also
show that whitespace may occur between the three elements. ::

   user=> (meta ^:keyword-as-meta-data [:vector :of :stuff])
   {:tag :keyword-as-meta-data}
   user=> (def symbol-as-meta-data 42)
   user=> (meta ^symbol-as-meta-data[:vector :of :stuff])
   {:tag 42}
   user=> (meta ^ "string as meta data"[:vector :of :stuff])
   {:tag "string as meta data"}
   user=> (meta ^{:map 1
                  :as 2
 		 :meta 3
 		 :data 4}
 		 [:vector :of :stuff])
   {:map 1, :as 2, :meta 3, :data 4}
   user=>

Clojure Conformance
-------------------

Same syntax.

#^
===

Exactly the same behavior as ^ but deprecated. (I may have this backwards, or
completely wrong)

Clojure Conformance
-------------------

Same syntax.

;
===

Read a single line comment.

Read and discard characters until a line terminator or the end of the stream
is reached.

Clojure Conformance
-------------------

Same syntax.

#=
===

Evaluate the next obj, before macro expansion.

Used internally by the core. The object following **#=** must be a symbol or a
list.

Clojure Conformance
-------------------

Same syntax.

#_
===

Read, then discard the next obj.

Provide syntax for *omitting* a single object::

   user=> [1 2 #_ 3 4 5]
   [1 2 4 5]
   user=>

The whitespace between **#_** and the object is not required::

   user=> [1 2 #_{:three 3 :four 4} 5]
   [1 2 4 5]
   user=>

Clojure Conformance
-------------------

Same syntax.

*******
Numbers
*******

A number must begin with [-+0-9]. No whitespace can occur anywhere in the
number. That includes between the optional sign and the first digit.

The reader will accept the following number formats.

Integral
========

Regardless of the format used, integral numbers are converted to base 10 and
returned as a Python int or long, depending on the size of the number.

* Base 10

  ``[+-]?(0|[1-9][0-9]*)``

  0, 1, -3, 4423423, +42, 1239485723094857203489572034897230834598843

* Base 8

  ``[+-]?0[0-7]+``

  0777, -042, +03234

* Base 16

  ``[+-]?0[xX][0-9a-fA-F]+``

  0x12, -0xff, +0xDEADbeef

* Base N
  ::

   [+-]?
   [1-9][0-9]?
   [rR]
   [0-9a-zA-Z]+

  The radix can be specified by a one or two digit base 10 number before
  the r. It must be in the range [2, 36] inclusive::

     user=> 2r1010101
     42
     user=> 36rZZZZ
     1679615
     user=>
  
Floating Point
==============
::

   [+-]?
   \d+
   (\.\d*([eE][+-]?\d+)?
    |
    [eE][+-]?\d+)

Python floating point syntax is used with one exception. A number must precede
the decimal.

Python does not require a leading digit in floating point numbers. clojure-py
does. In clojure-py .3, -.333, and +.001 are all symbols, not numbers. (See:
Symbols). A Python float instance will be returned.

Examples:

  0., -0. +0., 1e3, -1E-4, 2.2, -3.3e+9


Rational
========
::

   [+-]?
   (0|[1-9]+)
   /
   (0|[1-9]+)

Specified by a numerator and denominator seperated by a /. Both numerator and
denominator must be base 10 integers as described above.

Examples:

   1/2, -3/4, +2234/23342

For now, a Python fractions.Fraction will be returned. N/0 will match
successfully but raise an exception upon Fraction creation.

Clojure Conformance
-------------------

Clojure allows what *looks* like an octal number as either the numerator, or
the denominator::

   user=> 0777/1
   777
   user=>

clojure-py disallows this. Only base 10 integers are permissible.

Python Complex Numbers
======================

Not readable at the time of this writing. Reader syntax will have to be
discussed if these are allowed. Some examples:

  * Common Lisp
  
    * #c(r, i)
    * #C(r, i)
    
  * Scheme

    A single token is used with quite a bit of syntax within it:
    
    * +i, -i
    * 2+i, 2-i
    * 2+3i, 2-3i
    * +3i, -3i

    This would actually fit in because as stated at the beginning of this
    section: A number must begin with: [-+0-9].

Clojure Conformance
===================

* The optional posfix M is not allowed

* Numerator and denominator must be base 10 integers (See: Rational)

*******
Symbols
*******

The last thing the reader looks for is a symbol. This means, with a few
exceptions, that a symbol can be any string of adjacent characters that cannot
be interpreted any other way; (list), "string"", etc.

The exceptions are: " ; ' \\ @ ^ ` ~ % ( ) [ ] { } whitespace

% is a special case. A symbol may begin with %, but only if it does not occur
in an anonymous function (See: **#()**). % may not occur anywhere else in the
symbol.

Some symbols are reserved: **nil**, **true**, and **false**. The reader will
return Python None, True, and False, respectively.

To check the validity of a symbol, the reader has three major check-points in
the following order:

1. The readToken(rdr, initch) function

   This collects characters until the end of the input stream is reached, a
   whitespace character is read, or a terminating macro character is read. A
   terminating macro character is defined as any key in the lispreader.macros
   dict except "#". This is where all of the exceptions mentioned above come
   from.

   The result of this is passed to ...

2. The interpretToken(s) function

   This simply checks for a few special cases such as the reserved symbols
   mentioned above. So true, false, and nil aren't really symbols. They are
   more like constants.

   If this function fails to find a match, s is passed to ...

3. The matchSymbol(s) function

   This matches the token string s against symbolPat::

      [:]?           optional :
      ([^\d/].*/)?   optional namespace
                       * can't start with a digit or a /
		       * must end with a /
		       * minimum two characters in length including the /
      ([^\d/][^/]*)  name
                       * can't start with a digit or a /
		       * must not contain a /
		       * minumum one character in length
      

   which is only a *rough* estimate of what a symbol should look like. If that
   matches we just might have a valid symbol!  But not yet ...

   Now the function starts pulling s apart. It first checks if we have a
   namespace and that the namespace does not end in ":/". Then it makes sure
   name does not end in ":". Finally it makes sure that if "::" is present in
   the token, it occurs only at the beginning.

   Ok. Now it checks if the token starts with "::". If so and namespace is
   present, it looks up the namespace. If the compiler can't find the
   namespace, the function fails and returns None. This is actually a symantic
   error, not a syntax error.

   So, if the token does not start with "::", does it start with ":"? If so it
   *should* be a valid keyword. Construct one and return it.

   No ":" at the beginning of the token? Construct a symbol and return it.

   If the function has not returned by now, it's not a valid symbol. Return
   None.

Clojure Conformance
===================
 
Same syntax (AFAIK).
