#! /usr/bin/env python
'''
what to do:
    create summary.markdown for gitbook
how to use:
    cd zh-cn && python summary.py
    gitbook init && gitbook build && gitbook pdf
reuslt:
    it works, anyway.

walker.zheng[mykulou@gmail.com]
'''

import os
import copy
import re

rootdir = os.path.abspath(os.curdir)

lastDir = ""
md = re.compile(r'.*\.(markdown|md)$')
_book = re.compile(r'_book|SUMMARY')
ext = re.compile(r'\.html\.(markdown|md)')
files = []
summary = []
readme = []

# get all file path
for parent,dirnames,filenames in os.walk(rootdir):
    for filename in filenames:
        files.append(os.path.join(parent, filename))

# create md content tree
for line in files:
    b = _book.search(line)
    if not b :
        m = md.match(line)
        if m :
            local = line.replace(rootdir + "/", "")
            n = local.rfind("/")
            name = ext.sub("", local)
            if n > 0 :
                if lastDir != local[:n]:
                    lastDir = local[:n]
                    summary.append("* [" + name + "](" + local + ")\n")
                summary.append("\t* [" + name + "](" + local + ")\n")
            else:
                summary.append("* [" + name + "](" + local + ")\n")

def write2file(content, path):
        with open(path, "w") as f:
            for line in content:
                f.write(line)

readme = copy.deepcopy(summary)
summary.insert(0, """learnxinyminutes-docs
=======

# SUMMARY
""")
readme.insert(0, """learnxinyminutes-docs
=======

# Introduction
""")

#write2file(readme, "README.markdown")
write2file(summary, "SUMMARY.markdown")


