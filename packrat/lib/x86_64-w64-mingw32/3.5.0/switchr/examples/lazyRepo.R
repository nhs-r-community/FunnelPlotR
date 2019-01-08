library(switchr)

  ghman = GithubManifest("gmbecker/rpath", "hadley/lazyeval",
      "hadley/dplyr", "rstudio/ggvis")

res = lazyRepo("ggvis", pkg_manifest = ghman)

  install_packages(c("Rcpp", "ggvis"), ghman)
