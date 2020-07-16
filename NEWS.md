# FunnelPlotR 0.2.9999

- Added methods for ratio of counts and proportions
- Renamed several arguments
- Added 'themes' as people don't like the ggplot grey as default
- Broke process into much smaller functions for easier editing and speed
- Tweaked vignette to make it work
- Removed unecceary rlang dependency

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
