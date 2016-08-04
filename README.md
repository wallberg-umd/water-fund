water-fund
==========

DSS bottled water usage report

Merge data from local GnuCash fund/account tracking and Deer Park invoices
* GnuCash export to sqlite (water charges and user contributions)
* PDFBox and getBottles.py to extract number of bottles from Deer Park PDF invoice

WaterFund.R to generate report

Procedure
---------

```
(cd ~/Documents/DeerPark; for f in *.pdf ; do pdfbox ExtractText $f; done)
./getBottles.py ~/Documents/DeerPark/*.txt > bottles.csv
```
