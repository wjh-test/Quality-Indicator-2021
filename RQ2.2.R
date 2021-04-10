setwd(".")
rm(list = ls(all = TRUE))
library("effsize")#for A12 test https://rdrr.io/cran/effsize/man/VD.A.html

data <- read.table(file = "data/inputDataDiffStructureQI.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C", "ND")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")
#Problems <- c("RA", "TS", "TRA", "RP", "TM", "TP1", "TP2_1", "TP2_2", "TP2_3", "RM", "ITO", ...)
Problems <- as.vector(unique(data$Problem))

overallcount <-0

dataDiffStructure <- data.frame()
for (p in Problems)
{
  for (alg in ALGs)
  {
    for (qi in QIs)
    {
      count <- NROW(subset(data,data$Problem==p & data$Algo==alg & data$QI==qi))
      countQI <- (8-1)
      if (qi=='ND')
      {
        countQI <- 28#1+2+3+4+5+6+7=28
      }
      Percent <- count/countQI
      row <- data.frame(NameOfProblem = p, Algo = alg, QI=qi, Counter = count, Percentage = Percent, PercentageII = Percent*100)
      dataDiffStructure <- rbind(dataDiffStructure, row)
      overallcount<-overallcount+count
    }
  }
}
write.table(dataDiffStructure, file = "results/RQ2.2.txt", sep = "\t", quote = FALSE, row.names = FALSE)

for(a in ALGs) {
  algResults <- subset(dataDiffStructure,dataDiffStructure$Algo==a)
  write.table(algResults, file = paste0("results/byALGProblem/byALGProblem",a,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  for (qi in QIs) {
    algPresults <- subset(algResults,algResults$QI==qi)[,c(1,5,6)]
    write.table(algPresults, file = paste0("results/byALGProblem/byALGProblemSplit/byALGProblem",a,"_",qi,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  }
}

dataS <- read.table(file = "results/RQ2.2.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
dataStructureBetter <- data.frame()#only strictly better
dataStructureBetterEq <- data.frame()#better and equal
dataStructureEq <- data.frame()#only equal
for (alg in ALGs)
{
  dataALG <-subset(dataS, dataS$Algo==alg)
  for (qi1 in QIs)
  {
    q1data <- subset(dataALG, dataALG$QI==qi1)
    for (qi2 in QIs) {
      if (qi1 != qi2) {
        q2data <- subset(dataALG, dataALG$QI==qi2)
        
        UtestPvalue <- wilcox.test(q1data$Percentage, q2data$Percentage, exact = FALSE)$p.value
        A12est <- VD.A(q1data$Percentage, q2data$Percentage)$estimate #A12
        if (UtestPvalue < 0.05) {
          if ( A12est > 0.5)
          {
            row <- data.frame(Algo=alg, QI1=qi1, QI2=qi2, Preferred=qi1)
            dataStructureBetter <- rbind(dataStructureBetter, row)
            dataStructureBetterEq <- rbind(dataStructureBetterEq, row)
          }
        }
        else {
          row <- data.frame(Algo=alg, QI1=qi1, QI2=qi2, Preferred=qi1)
          dataStructureBetterEq <- rbind(dataStructureBetterEq, row)
          dataStructureEq <- rbind(dataStructureEq, row)
        }
      }
    }
  }
}

write.table(dataStructureEq, file = "results/RQ2.2pvaluesEq.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetterEq, file = "results/RQ2.2pvaluesBetterEq.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetter, file = "results/RQ2.2pvaluesBetter.txt", sep = "\t", quote = FALSE, row.names = FALSE)

print(overallcount)