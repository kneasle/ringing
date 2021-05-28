#!/usr/bin/env python3

import sys

# Parse the call-string into a sequence of (call, position) pairs
callstring = sys.argv[1]
callpairs = []
call_so_far = ""
for c in callstring:
    if not c.isalpha():
        continue
    if c.islower():
        call_so_far += c
    else:
        call = "-" if call_so_far == "" else call_so_far
        callpairs.append((call, c))
        call_so_far = ""

# Convert to complib's layout
columns = list(set(map(lambda v: v[1], callpairs)))
lines = ["\t".join(columns)] + [
    "\t" * columns.index(position) + call for (call, position) in callpairs
]
print("\n".join(lines))
