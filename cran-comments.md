## Release summary
This is a minor bug-fix for the FunnelPlotR where support material and function defaults did not match.  Updated documentation, no other changes made.

## Test environments
* local windows 7, R 3.6.1
* local windows 10, R 3.6.1
* Windows dev on Win-builder
* ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1

* r-hub:
  * Ubuntu Linux 16.04 LTS, R-release, GCC, fedora-clang-devel
  * Fedora Linux, R-devel, clang, gfortran
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results
There were no ERRORs or WARNINGs.

NOTES on win-builder and r-hub about possibly invalid links.
These refer to DOIs for references in the vignette.  I've manually tested all links and they are valid.

Further note on win-builder and some r-hub builds about a CRAN reference not being in canonical form.  This has been changed to use https as suggested.


## Downstream dependencies
There are currently no downstream dependencies for this package.
