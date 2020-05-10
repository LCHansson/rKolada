## Resubmission
This is a resubmission. In this version I have:

* Shortened the Title field in DESCRIPTION to 65 characters.

* Added undirected single quotes around non-English words in title and description fields in DESCRIPTION.

* Removed all \dontrun{} tags from code examples.

* Added missing \arguments Rd-tag to functions that accept arguments but were missing the proper documentation tag.

* Added missing \value Rd-tags to all functions except the pipe function which is imported from the magrittr package, where the official documentation also does not contain a \value tag.

* Ensured my functions do not write by default to the user's home filespace.

Many thanks for taking the time to review this package and for all the helpful comments I've received so far!

## Test environments
* local OS X install, R 3.6.0
* win-builder (oldrelease, release, devel)
* Ubuntu Linux 16.04 LTS, R-release, GCC (R-Hub)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Love Hansson <love.hansson@gmail.com>'

New submission

## Downstream dependencies
No downstream dependencies at this time.