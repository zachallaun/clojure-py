"""
Contains enhancements to the REPL that do not belong in the core language.
"""

import traceback

from clojure.lang.globals import currentCompiler
from clojure.lang.compiler import Compiler
from clojure.lang.symbol import symbol
from clojure.lang.var import Var, intern as internVar
from clojure.lang.lispreader import read
from clojure.lang.fileseq import StringReader


def enable_readline():
    """
    Imports the `readline` module to enable advanced repl text manipulation,
    and command history navigation.

    Returns True if success, otherwise False.
    """
    try:
        import readline
    except ImportError:
        return False

    import os
    import atexit
    histfile = os.path.join(os.path.expanduser("~"), ".clojurepyhist")
    if not os.path.isfile(histfile):
        with open(histfile, 'a'):
            os.utime(histfile, None)
        os.chmod(histfile, int('640',8))
    try:
        readline.read_history_file(histfile)
    except IOError:
        # Pass here as there isn't any history file, so one will be
        # written by atexit
        pass
    atexit.register(readline.write_history_file, histfile)
    return True

def run_repl(comp=None):
    """
    Starts the repl. Assumes that RT.init has allready be called.
    """
    if comp is None:
        curr = currentCompiler.get(lambda: None)
        if curr == None:
            comp = Compiler()
            currentCompiler.set(comp)
        else:
            comp = curr
    comp.setNS(symbol("user"))

    last3 = [None, None, None]

    def execute(string):
        r = StringReader(string)
        s = read(r, False, None, True)
        res = comp.compile(s)
        return comp.executeCode(res)

    while 1:
        for i, value in enumerate(last3, 1):
            sym = symbol('*%s' % i)
            v = internVar(comp.getNS(), sym)
            v.setDynamic(True)
            if isinstance(value, Var):
                v.bindRoot(value.deref())
                v.setMeta(value.meta())
            else:
                v.bindRoot(value)

        try:
            line = raw_input(comp.getNS().__name__ + "=> ")
        except EOFError:
            break

        if not line:
            continue

        while unbalanced(line):
            try:
                new_line = '\n' + raw_input('.' * len(comp.getNS().__name__) + '.. ')
            except EOFError:
                break

            if not new_line.strip().startswith(';'):
                line += new_line

        # Propogate break from above loop.
        if unbalanced(line):
            break

        try:
            out = execute(line)
        except Exception:
            traceback.print_exc()
        else:
            last3.pop()
            last3.insert(0, out)
            print out

def unbalanced(line):
    """
    Returns true if the parentheses in the line are unbalanced.
    """
    chars = (
        ("(", "[", "{"),
        (")", "]", "}"))
    # Counts for (, [, {, and "
    counts = [0, 0, 0]
    for c in line:
        for i, paren in enumerate(zip(*chars)):
            if c == paren[0]: # Paren open
                counts[i] += 1
            elif c == paren[1]: # Paren close
                counts[i] -=1
    for count in counts:
        if count != 0:
            return True
    return False
