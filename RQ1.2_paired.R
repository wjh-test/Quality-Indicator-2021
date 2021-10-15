setwd(".")
rm(list = ls(all = TRUE))
library("effsize")#for A12 test https://rdrr.io/cran/effsize/man/VD.A.html

statTest <- function(data1, data2, Algo1, Algo2, greaterBetter, QI, data1AllResults, data2AllResults) {
  #null hypothesis is that the populations are the same
  #if p-value is less than 0.05, we can reject the null hypothesis
  UtestPvalueUnpaired <- wilcox.test(data1, data2, exact = FALSE, paired = FALSE)$p.value
  A12estUnpaired <- VD.A(data1, data2, paired = FALSE)$estimate #A12
  PreferredUnpaired <- ifelse(UtestPvalueUnpaired >= 0.05, "EQUAL", ifelse(A12estUnpaired >= 0.638, ifelse(greaterBetter, Algo1, Algo2), ifelse(A12estUnpaired <= 1-0.638, ifelse(greaterBetter, Algo2, Algo1), "EQUAL")))
  
  UtestPvaluePaired <- wilcox.test(data1, data2, exact = FALSE, paired = TRUE)$p.value
  A12estPaired <- VD.A(data1, data2, paired = TRUE)$estimate #A12
  PreferredPaired <- ifelse(UtestPvaluePaired >= 0.05, "EQUAL", ifelse(A12estPaired >= 0.638, ifelse(greaterBetter, Algo1, Algo2), ifelse(A12estUnpaired <= 1-0.638, ifelse(greaterBetter, Algo2, Algo1), "EQUAL")))
  
  UtestPvalueUnpairedAll <- wilcox.test(data1AllResults, data2AllResults, exact = FALSE, paired = FALSE)$p.value
  A12estUnpairedAll <- VD.A(data1AllResults, data2AllResults, paired = FALSE)$estimate #A12
  PreferredUnpairedAll <- ifelse(UtestPvalueUnpairedAll >= 0.05, "EQUAL", ifelse(A12estUnpairedAll >= 0.638, ifelse(greaterBetter, Algo1, Algo2), ifelse(A12estUnpaired <= 1-0.638, ifelse(greaterBetter, Algo2, Algo1), "EQUAL")))
  
  row <- data.frame(QI, Algo1, Algo2, 
                    PreferredUnpaired, PreferredPaired, PreferredUnpairedAll,
                    UtestPvalueUnpaired, A12estUnpaired, UtestPvaluePaired, A12estPaired, UtestPvalueUnpairedAll, A12estUnpairedAll)
}

data <- read.table(file = "data/inputDataDiffStructure.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CellDE", "MOCell", "NSGA-II", "PAES", "SMPSO", "SPEA2")
Problems <- as.vector(unique(data$Problem))

dataS <- read.table(file = "results/RQ1.2.txt", head = TRUE)
ALGs <- c("CellDE", "MOCell", "NSGA-II", "PAES", "SMPSO", "SPEA2")
dataStructureBetterPairedUnpaired <- data.frame()#only strictly better
dataStructureBetterEqPairedUnpaired <- data.frame()#better and equal
dataStructureBetterPaired <- data.frame()#only strictly better
diffPairedUnpaired <- data.frame()
for (qi in QIs)
{
  dataQI <-subset(dataS, dataS$QI==qi)
  #for (alg1 in ALGs)
  for (i in c(1:(length(ALGs)-1)))
  {
    alg1 <- ALGs[i]
    a1data <- subset(dataQI, dataQI$Algo==alg1)

    nextIndex <- i+1
    #for (alg2 in ALGs)
    for (j in c(nextIndex:length(ALGs)))
    {
      alg2 <- ALGs[j]
      if (alg1 != alg2) {
        a2data <- subset(dataQI, dataQI$Algo==alg2)

        availableProblems <- intersect(unique(a1data$NameOfProblem), unique(a2data$NameOfProblem))
        a1dataSel <- subset(a1data, a1data$NameOfProblem %in% availableProblems)
        a2dataSel <- subset(a2data, a2data$NameOfProblem %in% availableProblems)
        a1dataSel <- a1dataSel[order(a1dataSel$NameOfProblem),]
        a2dataSel <- a2dataSel[order(a2dataSel$NameOfProblem),]
        
        
        row <- statTest(a1dataSel$Percentage, a2dataSel$Percentage, alg1, alg2, TRUE, qi, a1data$Percentage, a2data$Percentage)
        
        dataStructureBetterEqPairedUnpaired <- rbind(dataStructureBetterEqPairedUnpaired, row)
        if(!(row$PreferredPaired=="EQUAL")) {
          dataStructureBetterPairedUnpaired <- rbind(dataStructureBetterPairedUnpaired, row)
          rowFiltered <- data.frame(QI=row$QI, Algo1=row$Algo1, Algo2=row$Algo2, Preferred=row$PreferredPaired)
          dataStructureBetterPaired <- rbind(dataStructureBetterPaired, rowFiltered)
        }
        if(row$PreferredUnpairedAll!=row$PreferredPaired) {
          rowFilteredDiff <- data.frame(QI=row$QI, Algo1=row$Algo1, Algo2=row$Algo2, PreferredPaired=row$PreferredPaired, PreferredUnpaired=row$PreferredUnpairedAll)
          diffPairedUnpaired <- rbind(diffPairedUnpaired, rowFilteredDiff)
        }
      }
    }
  }
}
#allDiff <- subset(dataStructureBetterEqPaired,dataStructureBetterEqPairedUnpaired$PreferredUnpairedAll!=dataStructureBetterEqPaired$PreferredPaired)
#write.table(dataStructureBetterEqPairedUnpaired, file = "results/RQ1.2pvaluesBetterEq_UnpairedPaired.txt", sep = "\t", quote = FALSE, row.names = FALSE)
#write.table(dataStructureBetterPairedUnpaired, file = "results/RQ1.2pvaluesBetter_UnpairedPaired.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetterPaired, file = "results/RQ1.2pvaluesBetter_Paired.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(diffPairedUnpaired, file = "results/RQ1.2_diff_pair_unpaired.txt", sep = "\t", quote = FALSE, row.names = FALSE)
