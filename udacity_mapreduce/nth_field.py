#!/usr/bin/env python

import mrlib
import sys

inp = mrlib.in_tsv(sys.stdin)
outp = mrlib.out_tsv(sys.stdout)

mrlib.map_nth(inp, outp, 4)
