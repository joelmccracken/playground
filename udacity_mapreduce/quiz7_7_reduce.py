#!/usr/bin/env python

import mrlib
import sys


infile = mrlib.in_tsv(sys.stdin)
outfile = mrlib.out_tsv(sys.stdout)

this_word = None
this_count = 0
this_ids = []


def output(outfile, word, count, ids):
  outfile.writerow([word, count, ids])


for line in infile:
  word = line[0]
  id   = line[1]

  if word == this_word:
    this_count += 1
    this_ids.append(id)
  else:
    output(outfile, this_word, this_count, this_ids)
    this_word = word
    this_count = 1
    this_ids = [id]

output(outfile, this_word, this_count, this_ids)
