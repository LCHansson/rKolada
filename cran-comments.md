## Resubmission
This is a resubmission. In this version I have:

* removed a dead URL from Readme.md (this was causing a NOTE)

* added the official URL to the database to the package Description field

* removed an unnecessary "in R" from the package Title field

## Test environments
* local OS X install, R 4.0.3
* R-hub Fedora Linux, R-devel, clang, gfortran
* R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-09-20 as problems were not corrected
    in time.
    
  The problems mentioned in the NOTE have been corrected for this release. The package now fails gracefully on no internet connection and produces informative error messages.

## Downstream dependencies
No downstream dependencies at this time.