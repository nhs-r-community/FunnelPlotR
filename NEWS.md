# FunnelPlotR 0.4.2
- Changes:
* Handling of expected values in SHMI calculation was adjusted to allow rounding to two decimal places to match HED and NHSD.  Now controlled with additional argument: `SHMI_rounding` = TRUE/FALSE
* A change to the OD logic when OD adjustment is selected, but no OD detected.  Changed to focus on phi (dispersion ratio) instead of tau (variance component).


- Bug-fix: 
* an error handling function was not performance correctly and preventing multiple selections.
* Updated plotting as ggplot arguments were deprecated and giving warning notices.
* Updated package author's email address, as I lost access to the old one.
* Updated bits of meta data, codecov, roxygen2 version and github actions.
* Added additional unit tests.

# FunnelPlotR 0.4.1

- Bug-fix release: the outlier function was not passing the 'multiplier' argument and was wrong with anything except the default values.  This has been corrected to pass the multiplier properly.

# FunnelPlotR 0.4.0

- Major tidy up of the UI, inputs, and control options.  This is was to remove inconsistency and help with future development.
- Several arguments renamed, all old arguments should show deprecation warnings and are documented in help files. Notably:
  - `Poisson_limits` and `OD_adjusted` are now `draw_unadjusted` and `draw_adjusted`.
  - `label_outliers` is now `label` with with more options for labelling rules.
- New argument to highlight data points added.
- Correction for non-monotonic transformation of limits for proportions. Thanks @andrjohns.


# FunnelPlotR 0.3.2

- Bug fix for limits related to NHS Digital SHMI method, correcting truncation and rounding issue that led to wrong limits being too wide.
- Typos in support material

# FunnelPlotR 0.3.1

- Significantly reorganised the internals into more small functions
- Added methods for ratios of counts and proportions
- Renamed several arguments  -  please read the help file!
- Added S3 object system and associated methods
- Added 'themes' as people don't like the `ggplot2` grey as default
- Added ability to change colours
- Broke process into much smaller functions for easier editing and speed
- Tweaked vignettes for new methods and added new one showing plot options
- Removed unnecessary `rlang` dependency

# FunnelPlotR 0.2.3

- Minor bug fix for discrepancy between limit colours

# FunnelPlotR 0.2.2

- Minor bug fix for discrepancy between documentation and function defaults

# FunnelPlotR 0.2.1

- Added custom scaling arguments
- Removed the aggregation option.  It gives the same results for pre-aggregated data either way.
- Removed redundant pkgdown material
- Cleaned up directories and hidden internal functions
- New HEX Sticker! Some people didn't like the old colours so I've gone safer.

# FunnelPlotR 0.2.0

- This is the first CRAN release of FunnelPlotR, for building funnel plots of indirectly standardised ratios, such as hospital mortality ratio.  It allows adjustment of funnel limits for overdispersion.  Future releases will extend functionality to proportions and ratios of counts.
This package replaces the earlier CMFunnels package, that was only release on GitHub.
