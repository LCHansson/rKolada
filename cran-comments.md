## Test environments
* local OS X install, R 3.6.0
* win-builder (release, devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 3 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Love Hansson <love.hansson@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Kolada (3:29, 8:76)
  Kommun (8:84)
  LandstingsDatabasen (8:96)

Comment: These are in fact Swedish words. The package accesses a Swedish-only
database over the Internet.

* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  'cran-comments.md'

Comment: This file is included as advised in Hadley Wickham's book "R package
development" (http://r-pkgs.had.co.nz/release.html).


* checking R code for possible problems ... [5s] NOTE
get_ou: no visible binding for global variable 'municipality_id'
(...many more rows of the same kind of message follow)

Comment: This is because the package uses dplyr semantics to operate on columns
in tables downloaded from the Kolada database. Since the REST API used is stable
columns will always have the same names.


## Downstream dependencies
No downstream dependencies at this time.