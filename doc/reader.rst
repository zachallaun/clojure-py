###############################
The Lisp Reader (lispreader.py)
###############################

The purpose of the reader is to parse a stream of text, one character at a
time, and return the next clojure-py obj found.

**********
Whitespace
**********

The following characters are considered whitespace by the reader.

Space, Newline, Carriage Return, Tab, Backspace, Formfeed, and the Comma

Whitespace (including the comma) is used to delimit objs in the input
stream. It is then discarded. The exceptions include:

1. Literal Strings. All whitespace is retained.
2. Regular Expression Patterns. All whitespace is retained in the pattern
   string but ignored by the regex compiler.
3. Literal Characters. A single character following the backslash may be a
   whitespace character. It will be the character returned by the reader.

******************
Special Characters
******************

\\
===

Read a literal character.

Return a Python string of length one. The character following the \\ can be
specified in one of the following ways.

4. A single character::

   \x, \Q, \G, \\, \_, \}, \(, \λ, \,

5. A symbol

   The following symbols will be evaluated to their respective characters.
   
   * ``\space`` => ``" "``

   6. ``\newline`` => ``"\n"``

   7. ``\return`` => ``"\r"``

   8. ``\backspace`` => ``"\b"``

   9. ``\formfeed`` => ``"\f"``

   10. ``\tab`` => ``"\t"``
   
11. By unicode codepoint

   12. \\u followed by 4 hexidecimal digits

     ``\u03bb``

   13. \\U followed by 8 hexidecimal digits

     ``\U000003bb``

14. By octal value

   \\oOOO

   The \\o must be followed by exactly three octal digits. The value must be <=
   0377 octal (255 decimal).

   This deviates from Python's string \\ooo syntax because Python allows one
   to three octal digits following the \\. Thus, Python will interpret \\0 as
   an octal value (the NUL character), while clojure-py will evaluate \\0 to
   the character 0 (Zero). Requiring three digits to be present is simply an
   attempt to be consistent with \\uHHHH, \\UHHHHHHHH, and \\xHH syntax.

   ::

      user=> \o40
      \space
      user=>

15. By hexidecimal value

   \\xHH

   The \\x must be followed by exactly two hexidecimal digits.

   ::

     user=> \x20
     \space
     user=>

Python Named Unicode Character Syntax
-------------------------------------

\\N{name}

This syntax has been omitted because of at least two *problems*:

   16. { } is map syntax
   17. Spaces may occur in the name::

         \N{LESS-THAN SIGN}.

      The character reader calls readToken() to slurp up the token following
      \\. It stops reading if *whitespace*, the end of input, or a terminating
      macro character is read.
      
Of course these *problems* arise out of trying to conform to Python syntax. It
could be implemented any number of ways. Or, for now, just punt.

%
===

Read:

18. an argument in an anonymous function (See: #())
19. a symbol (See: Symbols)
   
\`
===

Syntax-quote the next obj.

[ ]
===

Read a vector.

Zero or more objs may appear between the brackets.
   
( )
===

Read a list.

Zero or more objs may appear between the parentheses.
   
{ }
===

Read a map.

Zero or an even number of objs may appear between the braces. The objs
alternate, key, val, key, val ...
   
" "
===

Read a literal string.

Python string syntax is used. The string may be broken over multiple lines as
Python triple-quote strings. Newlines will be retained in the string.
   
#" "
====

Read a regular expression pattern.

Python re syntax is used. The string may be broken over multiple lines and
contain Python # comments. The string will be compiled with the re.X flag. The
pattern obj will be returned.

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
   
#{ }
====

Read a set.

Zero or more objs may appear between the braces.
   
#<
===

Throw an unconditional exception (unreadable obj follows).

#'
===

Shorthand for the special form **var**.

Return the list ::

   (var next-obj)

'
===

Shorthand for the special form **quote**.

return the list::

   (quote next-obj)
   
~
===

Shorthand for the special form **unquote**.


return the list::

   (unquote next-obj)
   
~@
===

Shorthand for the special form **unquote-splicing**.

return the list::

   (unquote-splicing next-obj)
   
@
===

Shorthand for the function **deref**.

return the list::

   (deref next-obj)
   
^
===

Read the next obj as meta data.

The obj following the ^ must be a *bound* symbol, keyword, string, or map. The
reader then consumes one more obj and attaches the meta data to that obj. This
last obj is returned.

20. read ^
21. read obj1
22. read obj2, attach obj1 as meta data
23. return obj2

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

#^
===

Exactly the same behavior as ^ but deprecated.

;
===

Read a single line comment.

Read and discard characters until a line terminator or the end of the stream
is reached.

#=
===

Evaluate the next obj, before macro expansion.

Used internally by the core. The object following **#=** must be a symbol or a
list.

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

*******
Numbers
*******

A number must begin with [-+0-9]. No whitespace can occur anywhere in the
number. That includes between the optional sign and the first digit.

The default reader will accept the following number formats.

Integral
========

Regardless of the format used, integral numbers are converted to base 10 and
returned as a Python int or long, depending on the size of the number.

   24. Base 10

     ``[+-]?(0|[1-9]+)``

     0, 1, -3, 4423423, +42, 1239485723094857203489572034897230834598843

   25. Base 8

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
   (\d+[Ee][+-]?\d+
    |
    \d+\.\d*
    ([Ee][+-]?\d+)?)

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
denominator must be base 10 integers as described above. A Python
fractions.Fraction will be returned. N/0 will match successfully but raise an
exception upon Fraction creation.

Examples:

   1/2, -3/4, +2234/23342

Imaginary
=========
::

   [+-]
   ((0|[1-9]+)
    |
    (\d+[Ee][+-]?\d+
     |
     \d+\.\d*
     ([Ee][+-]?\d+)?))
   [jJ]

Python syntax is used. A Base 10 integer or Floating Point number, as decribed
above, followed by a j or J. A Python complex instance is returned.

Examples:

   3j, -42.3J, 0J, 3e-4j

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

26. The readToken(rdr, initch) function

   This collects characters until the end of the input stream is reached, a
   whitespace character is read, or a terminating macro character is read. A
   terminating macro character is defined as any key in the lispreader.macros
   dict except "#". This is where all of the exceptions mentioned above come
   from.

   The result of this is passed to ...

27. The interpretToken(s) function

   This simply checks for a few special cases such as the reserved symbols
   mentioned above. So true, false, and nil aren't really symbols. They are
   more like constants.

   If this function fails to find a match, s is passed to ...

28. The matchSymbol(s) function

   This matches the token string s against symbolPat::

      [:]?           optional :
      ([^\d/].*/)?   optional namespace
                       * can't start with a digit or a /
		       29. must end with a /
		       30. minimum two characters in length including the /
      ([^\d/][^/]*)  name
                       * can't start with a digit or a /
		       31. must not contain a /
		       32. minumum one character in length
      

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
