## Release summary

This is a main release with a change to composition of the plots and changes for compatibility with ggplot 4.0


## Test environments
* local windows 10 22H2, R 4.5.1

* R-devel and release, on Win-builder

* GitHub CIs actions:
  * Mac OS x 12.7.4 21H123, R 4.3.3
  * Windows Server x64 2022, 10.0.20348, R 4.3.3
  * Ubuntu 20.04.6 LTS, R-devel 2024-04-12
  * Ubuntu 20.04.6 LTS, R 4.3.3

* r-hub v2 via GitHub actions:
  * Ubuntu Linux 22.04.5 LTS, R-release, GCC
  * Ubuntu Linux 22.04.5 LTS, R-devel 2025-07-06 r88390
  * Ubuntu-clang Linux 20.04.5 LTS, R-devel 2025-07-06 r88390 
  * Mac OS x 14.4.1 23E224, R 4.3.3
  * Mac OS-arm64 x 14.4.1 23E224, R-devel 2024-04-12
  * Fedora Linux, R-devel 2025-07-06 r88390, intel3
  * Windows Server 2022, R-devel 2024-04-12, 64 bit

## R CMD check results
There were no ERRORs, WARNINGs.  2 NOTES on winbuilder:
* "possibly invlaid URL"" (URL is correct and still valid)
* "Problems with news in 'NEWS.md':  Cannot extract version info from the following section titles:
    Changes" - not seen on all other versions, but NEWS formatting checked and verified.

## Downstream dependencies
There are currently no downstream dependencies for this package to my knowledge.
