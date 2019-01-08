library(switchr)
man = PkgManifest(name = "limma", url = biocReposForVers("3.3")[1], type = "bioc",
                  dep_repos = biocReposForVers("3.3"))

sman = SessionManifest(man, versions = c("limma" = "3.28.21"))

n


man2 = PkgManifest(name = "limma", type="bioc")
sman2 = SessionManifest(man2, versions = c("limma" = "3.28.21"))
switchTo("testeroo", seed = sman)
switchBack()
removeLib("testeroo")



bigman = loadManifest("~/Downloads/WO29074_RNAseqDataToBCE_SwitchrLibrary.rman",)
switchTo("bigtest", seed = bigman)
switchBack()
removeLib("bigtest")
