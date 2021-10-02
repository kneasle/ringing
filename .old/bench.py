#!/usr/bin/env python3
import os
import sys
import shutil

# `./bench.py`: Run the benchmark suite
# `./bench.py pin`: Pin the results of the last benchmark run
if len(sys.argv) >= 2 and sys.argv[1] == "pin":
    print("Pinned last benchmark run")
    shutil.copy(".benches_prev.json", ".benches_pinned.json")
else:
    os.system("cargo bench --bench examples")
