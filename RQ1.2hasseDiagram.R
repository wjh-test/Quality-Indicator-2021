library("parallel")
library("BiocGenerics")
library("graph")
library("grid")
library("Rgraphviz")
library("hasseDiagram")

QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CellDE", "MOCell", "NSGA-II", "PAES", "SMPSO", "SPEA2")

dataPvaluesBetter <- read.table(file = "results/RQ1.2pvaluesBetter.txt", head = TRUE)

for (qi in QIs)
{
  dataPvaluesBetterQI <- subset(dataPvaluesBetter, dataPvaluesBetter$QI==qi)
  dataQI <- matrix(data = FALSE, ncol = 6, nrow = 6)
  for (i in c(1:6))
  {
    for (j in c(1:6))
    {
      sel <- subset(dataPvaluesBetterQI, dataPvaluesBetterQI$Algo1==ALGs[i] & dataPvaluesBetterQI$Algo2==ALGs[j])
      if(NROW(sel)==1) {
        if(sel$Preferred==ALGs[i])
          dataQI[i,j] = TRUE
        else
          dataQI[j,i] = TRUE
      }
    }
  }
  png(file = paste0("results/RQ1.2plots/", "hasseDiagram", qi, ".png"))
  par(mar = c(0, 0, 0, 0))
  hasse(dataQI, ALGs, list(cluster = FALSE, transitiveReduction=TRUE))
  dev.off()
  
  png(file = paste0("results/RQ1.2plots/", "hasseDiagram", qi, "noTrans.png"))
  par(mar = c(0, 0, 0, 0))
  hasse(dataQI, ALGs, list(cluster = FALSE, transitiveReduction=FALSE))
  dev.off()
}