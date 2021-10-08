library("parallel")
library("BiocGenerics")
library("graph")
library("grid")
library("Rgraphviz")
library("hasseDiagram")

QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")

PAIRED = TRUE
if(PAIRED) {
  dataPvaluesBetter <- read.table(file = "results/RQ2.2pvaluesBetter_paired.txt", head = TRUE)
}else {
  dataPvaluesBetter <- read.table(file = "results/RQ2.2pvaluesBetter.txt", head = TRUE)
}

for (alg in ALGs)
{
  dataPvaluesBetterALG <- subset(dataPvaluesBetter, dataPvaluesBetter$Algo==alg)
  dataALG <- matrix(data = FALSE, ncol = 8, nrow = 8)
  for (i in c(1:8))
  {
    for (j in c(1:8))
    {
      sel <- subset(dataPvaluesBetterALG, dataPvaluesBetterALG$QI1==QIs[i] & dataPvaluesBetterALG$QI2==QIs[j])
      if(NROW(sel)==1) {
        if(sel$Preferred==QIs[i])
          dataALG[i,j] = TRUE
        else
          dataALG[j,i] = TRUE
      }
    }
  }
  png(file = paste0("results/RQ2.2plots/", ifelse(PAIRED,"paired/",""), "hasseDiagram", alg, ".png"))
  par(mar = c(0, 0, 0, 0))
  hasse(dataALG, QIs, list(cluster = FALSE, transitiveReduction=TRUE))
  dev.off()
  
  png(file = paste0("results/RQ2.2plots/", ifelse(PAIRED,"paired/",""), "hasseDiagram", alg, "noTrans.png"))
  par(mar = c(0, 0, 0, 0))
  hasse(dataALG, QIs, list(cluster = FALSE, transitiveReduction=FALSE))
  dev.off()
}