import re
import os.path
import csv
import sys

def in_tsv_f(name):
  return in_tsv(open(name, 'rb'))

def out_tsv_f(name):
  return out_tsv(open(name, 'w'))

def out_tsv(outfd):
  return csv.writer(outfd, delimiter='\t', quotechar='"', quoting=csv.QUOTE_ALL)

def in_tsv(infd):
  return csv.reader(infd, delimiter='\t')

def map_f(infilename, outfilename, fn):
  if os.path.isfile(outfilename):
    print "skipping creation of %s, already exists" % outfilename
  else:
    inp = in_tsv_f(infilename)
    outp = out_tsv_f(outfilename)
    return map(inp, outp, fn)

def map(inp, outp, fn):
  for line in inp:
    outp.writerow(fn(line))

def map_nth_f(infilename, outfilename, number):
  map_f(infilename, outfilename, lambda x: [x[number]])

def map_nth(infd, outfd, number):
  return map(infd, outfd, lambda x: [x[number]])
