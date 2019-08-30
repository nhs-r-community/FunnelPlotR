## Release summary
This is the first release of a package wrapper for ggplot2 to draw funnel plots for indirectly standardised ratios, such as those used to compare mortality in hospitals against predicted rates. It processes input data and adjusts for overdispersion before drawing a ggplot2 object and returning relevant data.frames.  Future updates will include methods for proportions and ratios of counts.

## Test environments
* local windows 7, R 3.6.1
* local windows 10, R 3.6.1
* ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1

* r-hub:
  * Ubuntu Linux 16.04 LTS, R-release, GCC, fedora-clang-devel
  * Fedora Linux, R-devel, clang, gfortran
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results
There were no ERRORs or WARNINGs.

* One note from r-hub windows and ubuntu builds:
 "Found the following (possibly) invalid URLs:"

All links checked and are valid.  

## Downstream dependencies
There are currently no downstream dependencies for this package.
