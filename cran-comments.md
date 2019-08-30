## Release summary
This is the first release of a package wrapper for ggplot2 to draw funnel plots for indirectly standardised ratios, such as those used to compare mortality in hospitals against expected rates. It processes input data and adjusts for overdispersion before drawing a ggplot2 object and returning data tables.  Future updates will inlcude methods for proportions and ratios of counts.

## Test environments
* local windows 7, R 3.6.1
* local windows 10, R 3.6.1
* ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1
* r-hub:
  * Ubuntu Linux 16.04 LTS, R-release, GCC, fedora-clang-devel
  * Fedora Linux, R-devel, clang, gfortran

Currently error on Solaris via R-hub, but nowhere else as 'Modelmetrics' package, used in a vignette, is not available on their Solaris R build. Builds correctly with no errors on all other platforms.

## R CMD check results
There were no ERRORs, WARNINGs OR NOTES.

## Downstream dependencies
There are currently no downstream dependencies for this package.
