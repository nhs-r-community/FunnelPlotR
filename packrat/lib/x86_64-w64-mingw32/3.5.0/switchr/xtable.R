
library(switchr)


man = PkgManifest(name = "xtable", url = "svn://scm.r-forge.r-project.org/svnroot/xtable", subdir = "pkg", type="svn")
sman = SessionManifest(man, versions=c(xtable="1.8-3"))

rep = lazyRepo(sman)
