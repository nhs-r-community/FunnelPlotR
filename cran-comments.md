## Release summary

This is a main release with a change to the UI and some of the logic of the main plotting function:
* Implements tidyeval style interface to allow users to 'pipe' into it from dplyr
* Adjusted support material to reflect that.
* Updated control limit logical to allow turning in all limits.

Resolved 2 CRAN checks NOTES: lost braces in a file due to line breaks, and renamed arguments were missed in one function leading to 'documented arguments not in \usage'.  Apologies for the delay, the CRAN emails were filter to my junk folder and hadn't spotted them until this week.


## Test environments
* local windows 10, R 4.3.2

* R-devel and release, on Win-builder

* GitHub actions:
  * Mac OS x 12.7.4 21H123, R 4.3.3
  * Windows Server x64 2022, 10.0.20348, R 4.3
  * Windows Server x64 latest, R 3.6.3
  * Ubuntu 20.04.6 LTS, R-devel 2023-05-31
  * Ubuntu 20.04.6 LTS, R 4.3.3

* r-hub v2 via GitHub actions:
  * Ubuntu Linux 22.04.4 LTS, R-release, GCC
  * Ubuntu-clang Linux 20.04.4 LTS, R-devel - failed due to errors with Cairo device on platform
  * Mac OS-arm x 14.4.1 23E224, R 4.3.3 - failed due to cache issues on platform, and Cairo device.
  * Fedora Linux, R-devel, clang, gfortran
  * Windows Server 2022, R-devel, 64 bit

## R CMD check results
There were no ERRORs, WARNINGs or NOTES.

## Downstream dependencies
There are currently no downstream dependencies for this package to my knowledge.
