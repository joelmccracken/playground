#!/usr/bin/env python

import mrlib
import sys


infile = mrlib.in_tsv(sys.stdin)
outfile = mrlib.out_tsv(sys.stdout)

this_weekday = None
this_count = 0
this_total = 0


def output(outfile, day, count, total):
  if count == 0:
    return
  avg = float(total) / float(count)
  outfile.writerow([day, count, avg])


for line in infile:
  weekday = line[0]
  amount  = line[1]

  if weekday == this_weekday:
    this_count += 1
    this_total += float(amount)
  else:
    output(outfile, this_weekday, this_count, this_total)
    this_weekday = weekday
    this_count = 1
    this_total = float(amount)

output(outfile, this_weekday, this_count, this_total)
