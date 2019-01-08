library(switchr)

removeLib("IRangesDev")
removeLib("IRangesDev2")
man = BiocSVNManifest()

## use whatever permanent directory you want to place checkouts in.
## You can then work on the working copies within that directory and
## rebuilding the lazyRepo will reflect those changes.
dir = "~/localcheckout1"
repo = lazyRepo("IRanges", man, dir = dir)
switchTo("IRangesDev")
install_packages("IRanges", repo)

library(IRanges)
sessionInfo()

switchBack()
switchTo("IRangesDev2")
repo = lazyRepo("IRanges", man, dir = dir)
install_packages("IRanges", repo)

library(IRanges)
sessionInfo()
switchBack()
switchTo("IRangesDev")
