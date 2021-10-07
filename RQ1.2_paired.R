setwd(".")
rm(list = ls(all = TRUE))
library("effsize")#for A12 test https://rdrr.io/cran/effsize/man/VD.A.html

statTest <- function(data1, data2, firstApproach, secondApproach, greaterBetter, QI) {
  #null hypothesis is that the populations are the same
  #if p-value is less than 0.05, we can reject the null hypothesis
  UtestPvalueUnpaired <- wilcox.test(data1, data2, exact = FALSE, paired = FALSE)$p.value
  A12estUnpaired <- VD.A(data1, data2, paired = FALSE)$estimate #A12
  PreferredUnpaired <- ifelse(UtestPvalueUnpaired >= 0.05, "EQUAL", ifelse(A12estUnpaired > 0.5, ifelse(greaterBetter, firstApproach, secondApproach), ifelse(greaterBetter, secondApproach, firstApproach)))
  UtestPvaluePaired <- wilcox.test(data1, data2, exact = FALSE, paired = TRUE)$p.value
  A12estPaired <- VD.A(data1, data2, paired = TRUE)$estimate #A12
  PreferredPaired <- ifelse(UtestPvaluePaired >= 0.05, "EQUAL", ifelse(A12estPaired > 0.5, ifelse(greaterBetter, firstApproach, secondApproach), ifelse(greaterBetter, secondApproach, firstApproach)))
  row <- data.frame(QI, firstApproach, secondApproach, PreferredUnpaired, PreferredPaired, UtestPvalueUnpaired, A12estUnpaired, UtestPvaluePaired, A12estPaired)
}

data <- read.table(file = "data/inputDataDiffStructure.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")
Problems <- as.vector(unique(data$Problem))

dataS <- read.table(file = "results/RQ1.2.txt", head = TRUE)
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")
dataStructureBetter <- data.frame()#only strictly better
dataStructureBetterEq <- data.frame()#better and equal
dataStructureEq <- data.frame()#only equal
stats <- data.frame()#stats
for (qi in QIs)
{
  dataQI <-subset(dataS, dataS$QI==qi)
  for (alg1 in ALGs)
  {
    a1data <- subset(dataQI, dataQI$Algo==alg1)

    for (alg2 in ALGs) {
      if (alg1 != alg2) {
        a2data <- subset(dataQI, dataQI$Algo==alg2)

        availableProblems <- intersect(unique(a1data$NameOfProblem), unique(a2data$NameOfProblem))
        a1dataSel <- subset(a1data, a1data$NameOfProblem %in% availableProblems)
        a2dataSel <- subset(a2data, a2data$NameOfProblem %in% availableProblems)
        a1dataSel <- a1dataSel[order(a1dataSel$NameOfProblem),]
        a2dataSel <- a2dataSel[order(a2dataSel$NameOfProblem),]
        row <- statTest(a1dataSel$Percentage, a2dataSel$Percentage, alg1, alg2, TRUE, qi)
        stats <- rbind(stats, row)
      }
    }
  }
}

#write.table(dataStructureEq, file = "results/RQ1.2pvaluesEq_UnpairedPaired.txt", sep = "\t", quote = FALSE, row.names = FALSE)
#write.table(dataStructureBetterEq, file = "results/RQ1.2pvaluesBetterEq_UnpairedPaired.txt", sep = "\t", quote = FALSE, row.names = FALSE)
#write.table(dataStructureBetter, file = "results/RQ1.2pvaluesBetter_UnpairedPaired.txt", sep = "\t", quote = FALSE, row.names = FALSE)
