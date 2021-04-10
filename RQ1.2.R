setwd(".")
rm(list = ls(all = TRUE))
library("effsize")#for A12 test https://rdrr.io/cran/effsize/man/VD.A.html

data <- read.table(file = "data/inputDataDiffStructure.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2", "ND")
#Problems <- c("RA", "TS", "TRA", "RP", "TM", "TP1", "TP2_1", "TP2_2", "TP2_3", "RM", "ITO", ...)
Problems <- as.vector(unique(data$Problem))

overallcount <-0

dataDiffStructure <- data.frame()
for (p in Problems)
{
  for (qi in QIs)
  {
    for (alg in ALGs)
    {
      count <- NROW(subset(data,data$Problem==p & data$QI==qi & data$Algo==alg))
      if (p=='RM') {
        if(alg=="ND") {
          Percent <- count/10#4+3+2+1=10
        }
        else {
          Percent <- count/4#5-1=4
        }
      }
      else if(p=='RALIC'|p=='WORD'|p=='NRL') {
        if(alg=="ND") {
          Percent <- count/6#3+2+1=6
        }
        else {
          Percent <- count/3#4-1=3
        }
      }
      else{
        if(alg=="ND") {
          Percent <- count/15#5+4+3+2+1=15
        }
        else {
          Percent <- count/5#6-1=5
        }
      }
      if(alg!="CELLDE"|alg!="SMPSO"|p!='RM'|p!='RALIC'|p!='WORD'|p!='NRL'){
        row <- data.frame(NameOfProblem = p, QI=qi, Algo = alg, Counter = count, Percentage = Percent, PercentageII = Percent*100)
        dataDiffStructure <- rbind(dataDiffStructure, row)
        overallcount<-overallcount+count
      }
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
            row <- data.frame(QI=qi, Algo1 = alg1, Algo2 = alg2, Preferred = alg1 )
            dataStructureBetter <- rbind(dataStructureBetter, row)
            dataStructureBetterEq <- rbind(dataStructureBetterEq, row)
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
