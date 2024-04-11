## Resubmission
In this version I have:

* Added package-internal data which is used in the vignettes
* Modified all code examples so they don't run when the Kolada REST API is unavailable

## Test environments
* local OS X install, R 4.3.2
* R-hub Fedora Linux, R-devel, clang, gfortran
* R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub Windows Server 2022, R-devel, 64 bit
* r-release-macosx-arm64|4.1.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* Package was archived on CRAN

  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2022-12-12 for policy violation.
  
    On Internet access.
    
  The problems mentioned in the NOTE have been corrected for this release. The package now fails gracefully on no internet connection and produces informative error messages. Vignettes and code examples don't fail if the REST API is down.

## Downstream dependencies
No downstream dependencies at this time.