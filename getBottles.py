#!/usr/bin/env python

from sys import argv
import re
import os

# Input: Deer Park PDF Invoice converted to text using PDFBox
# Output: CSV with date and bottle counts

pDate = re.compile(' *(\d{1,2})\/(\d{1,2}) ')
pBottle = re.compile(' *BOTTLE DEPOSIT: +(\d+) +(DELIVERED|CHARGED)')

print '"date","bottles"'
for fname in argv:
    with open(fname) as f:
        for line in f.readlines():
            
            m = pDate.match(line)
            if (m):
                date = "%s-%02d-%02d" % (os.path.basename(fname)[0:4], int(m.group(1)), int(m.group(2)))
            
            m = pBottle.match(line)
            if (m):
                count = int(m.group(1))
                print '"%s", "%d"' % (date, count)
