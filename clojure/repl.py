"""
Contains enhancements to the REPL that do not belong in the core language.
"""

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
