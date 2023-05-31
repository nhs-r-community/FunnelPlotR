## Release summary
This is a bug-fix release for the FunnelPlotR package:
* an error handling function was not performing correctly and preventing multiple selections.
* A logic step for dealing OD adjustment if there is no OD.
* Updated plotting as ggplot arguments were deprecated and giving warning notices.
* Updated package author's email address, as I lost access to the old one.
* Updated bits of meta data, codecov, roxygen2 version and github actions.
* Added additional unit tests.
* Update package author email address

*Please note: I have lost access to my previous email address, used as author and was not
able to change it.  This submission has a new email address, but I can't confirm from the old
one.  Please correspond with c.mainey1@nhs.net*

## Test environments
* local windows 10, R 4.2.2
* local windows 10, R 4.3

* R-devel, on Win-builder

* GitHub actions:
  * Mac OS x 12.6.5 21G531, R 4.3
  * Windows Server x64 2022, 10.0.20348, R 4.3
  * Windows Server x64 latest, R 3.6.3
  * Ubuntu 20.04.6 LTS, R-devel 2023-05-31  - Failed, Error in loadNamespace(x) : there is no package called ‘rcmdcheck’ - this appears to be missing the rlib github actions
  * Ubuntu 20.04.6 LTS, R 4.3

* r-hub:
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran
  * Windows Server 2022, R-devel, 64 bit

## R CMD check results
There were no ERRORs or WARNINGs.

All checks reported NOTE of new maintainer vs. old maintainer.
R-hub Linux NOTES:
'Skipping checking HTML validation: no command 'tidy' found'
'Skipping checking math rendering: package 'V8' unavailable'

R-hub Windows NOTES:
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
I am unable to find the files described above, and believe it's an artefact of the build on R-Hub.
No such NOTES on GitHub actions or local build.


## Downstream dependencies
There are currently no downstream dependencies for this package to my knowledge.
