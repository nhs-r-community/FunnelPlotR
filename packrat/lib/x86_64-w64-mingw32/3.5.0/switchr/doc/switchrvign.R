### R code from vignette source 'switchrvign.Rnw'

###################################################
### code chunk number 1: switchrvign.Rnw:45-48
###################################################
library(switchr)
switchrBaseDir(file.path(tempdir(), ".switchr"))
options(width=40, repos = c(CRAN="http://cran.rstudio.com"))


###################################################
### code chunk number 2: switchrvign.Rnw:55-56
###################################################
switchTo("vign1")


###################################################
### code chunk number 3: switchrvign.Rnw:66-72
###################################################
switchBack()
txtcon = textConnection("txt", "w", local=TRUE)
sink(txtcon, type="message")
switchTo("vign1")
sink(NULL, type="message")
print(txt,width=30)


###################################################
### code chunk number 4: switchrvign.Rnw:83-84
###################################################
switchBack()


###################################################
### code chunk number 5: switchrvign.Rnw:104-106 (eval = FALSE)
###################################################
## ### Not Run
## removeLib("mylibrary")


###################################################
### code chunk number 6: switchrvign.Rnw:125-129
###################################################
man = PkgManifest(name = "fastdigest",
    url = "https://github.com/gmbecker/fastdigest",
    type = "git")
man


###################################################
### code chunk number 7: switchrvign.Rnw:136-139
###################################################
man2 = GithubManifest("gmbecker/fastdigest",
     "gmbecker/RCacheSuite")
man2


###################################################
### code chunk number 8: switchrvign.Rnw:150-153
###################################################
man3 = GithubManifest(redland = "ropensci/redland-bindings/R/redland")
man3



###################################################
### code chunk number 9: switchrvign.Rnw:162-164 (eval = FALSE)
###################################################
## ## NOT RUN due to peculiarities of CRAN build system wrt installed.packages()
## lman = libManifest()


###################################################
### code chunk number 10: switchrvign.Rnw:178-180 (eval = FALSE)
###################################################
## ## NOT RUN
## switchTo("vign2", seed = lman)


###################################################
### code chunk number 11: switchrvign.Rnw:198-200 (eval = FALSE)
###################################################
## ## NOT RUN
## install_packages("RCacheSuite", man2)


###################################################
### code chunk number 12: switchrvign.Rnw:206-211 (eval = FALSE)
###################################################
## ## NOT RUN
## install_packages("fastdigest",
##                  versions = c(fastdigest= "0.5-0"),
##                  man = man2)
## 


###################################################
### code chunk number 13: switchrvign.Rnw:236-238
###################################################
oldman = cranPkgVersManifest(pkg = "randomForest", vers = "4.6-5")
oldman


###################################################
### code chunk number 14: switchrvign.Rnw:247-248
###################################################
manifest_df(oldman)$url


###################################################
### code chunk number 15: switchrvign.Rnw:257-259
###################################################
oldman2 = rVersionManifest("3.1.1")
oldman2


###################################################
### code chunk number 16: switchrvign.Rnw:266-267
###################################################
head(manifest_df(oldman2)$url)


###################################################
### code chunk number 17: switchrvign.Rnw:280-282 (eval = FALSE)
###################################################
## ## NOT RUN
## switchrNoUnload(TRUE)


