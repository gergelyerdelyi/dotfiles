#!/usr/bin/env python
import sys, os, re
from subprocess import Popen, PIPE


def lint(filename):
    """
    This is a slightly modified version of epylint.py that comes with pylint
    removing the directory traversal logic of the original version.
    """
    # traverse downwards until we are out of a python package
    fullPath = os.path.abspath(filename)
    parentPath, childPath = os.path.dirname(fullPath), os.path.basename(fullPath)

    # Start pylint
    process = Popen('pylint -f parseable -r n --disable=C,R,I "%s"' %
                    childPath, shell=True, stdout=PIPE, stderr=PIPE,
                    cwd=parentPath)
    p = process.stdout

    # The parseable line format is '%(path)s:%(line)s: [%(sigle)s%(obj)s] %(msg)s'
    # NOTE: This would be cleaner if we added an Emacs reporter to pylint.reporters.text ..
    regex = re.compile(r"\[(?P<type>[WE])(?P<remainder>.*?)\]")

    def _replacement(mObj):
        "Alter to include 'Error' or 'Warning'"
        if mObj.group("type") == "W":
            replacement = "Warning"
        else:
            replacement = "Error"
        # replace as "Warning (W0511, funcName): Warning Text"
        return "%s (%s%s):" % (replacement, mObj.group("type"), mObj.group("remainder"))

    for line in p:
        # remove pylintrc warning
        if line.startswith("No config file found"):
            continue
        line = regex.sub(_replacement, line, 1)
        # # modify the file name thats output to reverse the path traversal we made
        # parts = line.split(":")
        # if parts and parts[0] == childPath:
        #     line = ":".join([filename] + parts[1:])
        print line,

    p.close()

if __name__ == '__main__':
    lint(sys.argv[1])

