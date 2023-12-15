## Test environments

* Windows 10, R version 4.2.3 (2023-03-15 ucrt)
* win-builder, R Under development (unstable) (2023-09-12 r85134 ucrt)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is considered a new release, against the previously published CRAN version, 0.55, due to a change in maintainer.

* Possibly misspelled words in DESCRIPTION:
  Biplots (3:42)
  biplot (17:5)
  biplots (12:50)
 
These are all spurious false alarms, owing to win-builder not recognizing the spelling WORDFILE.

I also updated the Date: field in DESCRIPTION

