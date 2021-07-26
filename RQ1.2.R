setwd(".")
rm(list = ls(all = TRUE))
library("effsize")#for A12 test https://rdrr.io/cran/effsize/man/VD.A.html

data <- read.table(file = "data/inputDataDiffStructure.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")
Problems <- as.vector(unique(data$Problem))

overallcount <-0

dataDiffStructure <- data.frame()
for (p in Problems)
{
  for (qi in QIs)
  {
    for (alg in ALGs)
    {
      if(alg=="CELLDE"&&p=='RM')
        next
      if((alg=="CELLDE"|alg=="SMPSO")&&(p=='RALIC'|p=='WORD'|p=='NRL'))
        next
      count <- NROW(subset(data,data$Problem==p & data$QI==qi & data$Algo==alg))
      den <- NROW(subset(data,data$Problem==p & data$QI==qi & (data$Alg1==alg | data$Alg2==alg)))
      Percent <- count/den
      row <- data.frame(NameOfProblem = p, QI=qi, Algo = alg, Counter = count, Percentage = Percent, PercentageII = Percent*100)
      dataDiffStructure <- rbind(dataDiffStructure, row)
      overallcount<-overallcount+count
    }
  }
}
write.table(dataDiffStructure, file = "results/RQ1.2.txt", sep = "\t", quote = FALSE, row.names = FALSE)

for (qi in QIs) {
  qiResults <- subset(dataDiffStructure,dataDiffStructure$QI==qi)
  write.table(qiResults, file = paste0("results/byQIProblem/byQIProblem",qi,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  for(a in ALGs) {
    qiPresults <- subset(qiResults,qiResults$Algo==a)[,c(1,5,6)]
    write.table(qiPresults, file = paste0("results/byQIProblem/byQIProblemSplit/byQIProblem",qi,"_",a,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  }
}

dataS <- read.table(file = "results/RQ1.2.txt", head = TRUE)
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")
dataStructureBetter <- data.frame()#only strictly better
dataStructureBetterEq <- data.frame()#better and equal
dataStructureEq <- data.frame()#only equal
min <- 1.0
for (qi in QIs)
{
  dataQI <-subset(dataS, dataS$QI==qi)
  for (alg1 in ALGs)
  {
    a1data <- subset(dataQI, dataQI$Algo==alg1)
    for (alg2 in ALGs) {
      if (alg1 != alg2) {
        a2data <- subset(dataQI, dataQI$Algo==alg2)
        
        UtestPvalue <- wilcox.test(a1data$Percentage, a2data$Percentage, exact = FALSE)$p.value
        A12est <- VD.A(a1data$Percentage, a2data$Percentage)$estimate #A12
        if (UtestPvalue < 0.05) {
          if ( A12est > 0.5)
          {
            row <- data.frame(QI=qi, Algo1 = alg1, Algo2 = alg2, Preferred = alg1)
            dataStructureBetter <- rbind(dataStructureBetter, row)
            dataStructureBetterEq <- rbind(dataStructureBetterEq, row)
            if(A12est < min) min <- A12est
          }
        }
        else {
          row <- data.frame(QI=qi, Algo1 = alg1, Algo2 = alg2, Preferred = alg1)
          dataStructureBetterEq <- rbind(dataStructureBetterEq, row)
          dataStructureEq <- rbind(dataStructureEq, row)
        }
      }
    }
  }
}

write.table(dataStructureEq, file = "results/RQ1.2pvaluesEq.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetterEq, file = "results/RQ1.2pvaluesBetterEq.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetter, file = "results/RQ1.2pvaluesBetter.txt", sep = "\t", quote = FALSE, row.names = FALSE)

print(overallcount)
print(min)
