"""Contains enhancements to the REPL that do not belong in the core language.
"""

import atexit
import os
import sys
import traceback

from clojure.lang.compiler import Compiler
from clojure.lang.fileseq import StringReader
from clojure.lang.globals import currentCompiler
from clojure.lang.lispreader import read
from clojure.lang.symbol import Symbol
from clojure.lang.var import Var, find as findVar
import clojure.lang.rt as RT
from clojure.main import VERSION_MSG


def enable_readline():
    """Imports the `readline` module to enable advanced REPL text manipulation
    and command history navigation.

    Returns True on success, otherwise False.
    """
    try:
        import readline
    except ImportError:
        return False

    histfile = os.path.join(os.path.expanduser("~"), ".clojurepyhist")
    if not os.path.isfile(histfile):
        with open(histfile, 'a'):
            os.utime(histfile, None)
        os.chmod(histfile, int('640', 8))
    try:
        readline.read_history_file(histfile)
    except IOError:
        # Pass here as there isn't any history file, so one will be
        # written by atexit
        pass
    atexit.register(readline.write_history_file, histfile)
    return True


def run_repl(opts, comp=None):
    """Initializes and runs the REPL. Assumes that RT.init has been called.

    Repeatedly reads well-formed forms from stdin (with an interactive prompt
    if a tty) and evaluates them (and prints the result if a tty). Exits on
    EOF.
    """
    if not opts.quiet and os.isatty(0):
        print VERSION_MSG

    if comp is None:
        curr = currentCompiler.get(lambda: None)
        if curr == None:
            comp = Compiler()
            currentCompiler.set(comp)
        else:
            comp = curr
    comp.setNS(Symbol("user"))
    core = sys.modules["clojure.core"]
    for i in dir(core):
        if not i.startswith("_"):
            setattr(comp.getNS(), i, getattr(core, i))

    line = opts.cmd
    last3 = [None, None, None]

    def firstLinePrompt():
        return comp.getNS().__name__ + "=> " if os.isatty(0) else ""

    def continuationLinePrompt():
        return "." * len(comp.getNS().__name__) + ".. " if os.isatty(0) else ""

    while True:
        # Evaluating before prompting caters for initially given forms.
        r = StringReader(line)
        while True:
            try:
                s = read(r, False, None, True)
                if s is None:
                    break
                res = comp.compile(s)
                out = comp.executeCode(res)
            except Exception:
                traceback.print_exc()
            else:
                if os.isatty(0):
                    RT.printTo(out)
                last3.pop()
                last3.insert(0, out)
                for i, value in enumerate(last3, 1):
                    v = findVar(Symbol("clojure.core", "*%s" % i))
                    if isinstance(value, Var):
                        v.bindRoot(value.deref())
                        v.setMeta(value.meta())
                    else:
                        v.bindRoot(value)
        try:
            line = raw_input(firstLinePrompt())
            while unbalanced(line):
                line += "\n" + raw_input(continuationLinePrompt())
        except BracketsException as exc:
            print exc
            continue
        except EOFError:
            print
            break


class BracketsException(Exception):
    """Raised in case of non-matching brackets in a line.
    
    Takes a single argument, the unmatched bracket.
    """
    def __str__(self):
        return "Unmatched delimiter '{0}'".format(self.args[0])


def unbalanced(line):
    """Returns whether the brackets in the line are unbalanced.

    Raises BracketsError in case of matching error.
    """
    ignore_pairs = '""', ";\n"
    ignore_closer = ""
    bracket_pairs = "()", "[]", "{}"
    stack = []

    for c in line:
        if ignore_closer:
            if c == ignore_closer:
                ignore_closer = ""
            else:
                continue
        else:
            for op, cl in ignore_pairs:
                if c == op:
                    ignore_closer = cl
                    continue
        for op, cl in bracket_pairs:
            if c == op:
                stack.append(cl)
                continue
            if c == cl:
                if not stack or stack.pop() != c:
                    raise BracketsException(c)
    return bool(stack)
