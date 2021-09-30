## Release summary
This is a bug-fix release for the FunnelPlotR package, as a labelling function was not passing a 'multiplier' argument correctly, and was wrong when anything except the default value was used. This is a key function, as one common metric that uses these plots is generally multiplied by 100.

## Test environments
* local windows 10, R 4.1.1

* R-devel, on Win-builder

* GitHub actions:
  * Mac OS x 10.15.7 19H1323, R 4.1.1
  * Windows Server x64 2019, R 4.1.1
  * Windows Server x64 latest, R 3.6.3
  * Ubuntu 20.04.3 LTS, R-devel 2021-09-11 r80887
  * Ubuntu 20.04.3 LTS, R 4.1.1

* r-hub:
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * x86_64-pc-linux-gnu
  * Fedora Linux, R-devel, clang, gfortran
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results
There were no ERRORs or WARNINGs.

NOTEs for 'possibly invalid URL' have all been checked and are valid.


## Downstream dependencies
There are currently no downstream dependencies for this package to my knowledge.
