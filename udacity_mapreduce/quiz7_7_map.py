#!/usr/bin/env python

import re
import os.path
import csv
import sys
import mrlib

splitter = re.compile(r'[\s\.,!\?"\(\)<>\[\]\#$=\-/]')
ignored = ['p', '', '1', '2', ";"]
# forum_posts = in_tsv('forum_node.tsv')
# forum_post_bodies = out_tsv('forum_bodies.tsv')

infile = mrlib.in_tsv(sys.stdin)
outfile = mrlib.out_tsv(sys.stdout)

for line in infile:
  id   = line[0]
  body = line[4]

  words = splitter.split(body)

  for word in words:
    word = word.lower()
    if word in ignored:
      continue
    outfile.writerow([word, id])
