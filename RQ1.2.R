setwd(".")
rm(list = ls(all = TRUE))
library("effsize")#for A12 test https://rdrr.io/cran/effsize/man/VD.A.html

data <- read.table(file = "data/inputDataDiffStructure.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CellDE", "MOCell", "NSGA-II", "PAES", "SMPSO", "SPEA2")
Problems <- as.vector(unique(data$Problem))

overallcount <-0

dataDiffStructure <- data.frame()
for (p in Problems)
{
  for (qi in QIs)
  {
    for (alg in ALGs)
    {
      if(alg=="CellDE"&&p=='RM')
        next
      if((alg=="CellDE"|alg=="SMPSO")&&(p=='RALIC'|p=='WORD'|p=='NRL'))
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
  for(p in Problems) {
    pResults <- subset(qiResults,qiResults$NameOfProblem==p)[,c(3,5,6)]
    pResults <- pResults[order(-pResults$Percentage),]
    write.table(pResults, file = paste0("results/byQIProblem/byQIProblemSplit/byQIProblem",qi,"_",p,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  }
  for(a in ALGs) {
    qiPresults <- subset(qiResults,qiResults$Algo==a)[,c(1,5,6)]
    write.table(qiPresults, file = paste0("results/byQIProblem/byQIALGSplit/byQIProblem",qi,"_",a,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  }
}


dataS <- read.table(file = "results/RQ1.2.txt", head = TRUE)
ALGs <- c("CellDE", "MOCell", "NSGA-II", "PAES", "SMPSO", "SPEA2")
dataStructureBetter <- data.frame()
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
        
        UtestPvaluePaired <- wilcox.test(a1dataSel$Percentage, a2dataSel$Percentage, exact = FALSE, paired = TRUE)$p.value
        A12estPaired <- VD.A(a1dataSel$Percentage, a2dataSel$Percentage, paired = TRUE)$estimate #A12
        PreferredPaired <- ifelse(UtestPvaluePaired >= 0.05, "EQUAL", ifelse(A12estPaired >= 0.638, ifelse(TRUE, alg1, alg2), ifelse(A12estPaired <= 1-0.638, ifelse(TRUE, alg2, alg1), "EQUAL")))
        
        if(!(PreferredPaired=="EQUAL")) {
          row <- data.frame(QI=qi, Algo1=alg1, Algo2=alg2, Preferred=PreferredPaired)
          dataStructureBetter <- rbind(dataStructureBetter, row)
        }
      }
    }
  }
}

write.table(dataStructureBetter, file = "results/RQ1.2pvaluesBetter.txt", sep = "\t", quote = FALSE, row.names = FALSE)
