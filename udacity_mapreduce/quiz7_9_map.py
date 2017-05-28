#!/usr/bin/env python

import re
import os.path
import csv
import sys
import mrlib

# splitter = re.compile(r'[\s\.,!\?"\(\)<>\[\]\#$=\-/]')
# ignored = ['p', '', '1', '2', ";"]
# # forum_posts = in_tsv('forum_node.tsv')
# # forum_post_bodies = out_tsv('forum_bodies.tsv')

infile  = mrlib.in_tsv(sys.stdin)
outfile = mrlib.out_tsv(sys.stdout)
import datetime

for line in infile:
  date   = line[0]
  amount = line[4]

  date_parsed = datetime.datetime.strptime(date, '%Y-%m-%d')

  outfile.writerow([date_parsed.weekday(), amount])
