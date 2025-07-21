## Release summary

This is a main release with a change to composition of the plots and changes for compatibility with ggplot 4.0


## Test environments
* local windows 10 22H2, R 4.5.1

* R-devel and release, on Win-builder

* GitHub CIs actions:
  * Ubuntu 24.04.2 LTS, R 4.5.1

* r-hub v2 via GitHub actions: https://github.com/nhs-r-community/FunnelPlotR/actions/runs/16416067313
  * Ubuntu Linux 24.04.2 LTS, R-release
  * Ubuntu Linux 24.04.2 LTS, R 4.5.1 patched 2025-07-14 r88411
  * Ubuntu-clang Linux 24.04.2 LTS, R-devel 2025-07-15 r88411
  * Mac OS x 13.7.6 22H625, R-devel 2025-07-18 r88431
  * Mac OS-arm64 x 14.6.6 23H626, 2025-07-18 r88431
  * Fedora Linux 38, R-devel 2025-07-15 r88411
  * Windows Server 2022, 10.0.20348 R-devel 2025-07-18 r88431

## R CMD check results
There were no ERRORs, WARNINGs.  2 NOTES on winbuilder:
* NOTE: Found the following hidden files and directories:
  .lintr  -  now added to .RBuildignore and no further notes.

## Downstream dependencies
There are currently no downstream dependencies for this package to my knowledge.
