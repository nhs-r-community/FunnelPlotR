# FunnelPlotR 0.3.1

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
