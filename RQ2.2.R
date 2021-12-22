setwd(".")
rm(list = ls(all = TRUE))
library("effsize")#for A12 test https://rdrr.io/cran/effsize/man/VD.A.html

data <- read.table(file = "data/inputDataDiffStructureQI.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CellDE", "MOCell", "NSGA-II", "PAES", "SMPSO", "SPEA2")
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
      if(alg=="CellDE"&&p=='RM')
        next
      if((alg=="CellDE"|alg=="SMPSO")&&(p=='RALIC'|p=='WORD'|p=='NRL'))
        next
      count <- NROW(subset(data,data$Problem==p & data$Algo==alg & data$QI==qi))
      den <- NROW(subset(data,data$Problem==p & data$Algo==alg & (data$QI1==qi | data$QI2==qi)))
      Percent <- count/den
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
  for(p in Problems) {
    if(a=="CellDE"&&p=='RM')
      next
    if((a=="CellDE"|a=="SMPSO")&&(p=='RALIC'|p=='WORD'|p=='NRL'))
      next
    pResults <- subset(algResults,algResults$NameOfProblem==p)[,c(3,5,6)]
    pResults <- pResults[order(-pResults$Percentage),]
    write.table(pResults, file = paste0("results/byALGProblem/byALGProblemSplit/byALGProblem",a,"_",p,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  }
  for (qi in QIs) {
    algPresults <- subset(algResults,algResults$QI==qi)[,c(1,5,6)]
    write.table(algPresults, file = paste0("results/byALGProblem/byALGQISplit/byALGProblem",a,"_",qi,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  }
}


dataS <- read.table(file = "results/RQ2.2.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
dataStructureBetter <- data.frame()
for (alg in ALGs)
{
  dataALG <-subset(dataS, dataS$Algo==alg)
  #for (qi1 in QIs)
  for (i in c(1:(length(QIs)-1)))
  {
    qi1 <- QIs[i]
    q1data <- subset(dataALG, dataALG$QI==qi1)
    
    nextIndex <- i+1
    #for (qi2 in QIs)
    for (j in c(nextIndex:length(QIs)))
    {
      qi2 <- QIs[j]
      if (qi1 != qi2) {
        q2data <- subset(dataALG, dataALG$QI==qi2)
        
        availableProblems <- intersect(unique(q1data$NameOfProblem), unique(q2data$NameOfProblem))
        q1dataSel <- subset(q1data, q1data$NameOfProblem %in% availableProblems)
        q2dataSel <- subset(q2data, q2data$NameOfProblem %in% availableProblems)
        q1dataSel <- q1dataSel[order(q1dataSel$NameOfProblem),]
        q2dataSel <- q2dataSel[order(q2dataSel$NameOfProblem),]
        
        UtestPvaluePaired <- wilcox.test(q1dataSel$Percentage, q2dataSel$Percentage, exact = FALSE, paired = TRUE)$p.value
        A12estPaired <- VD.A(q1dataSel$Percentage, q2dataSel$Percentage, paired = TRUE)$estimate #A12
        PreferredPaired <- ifelse(UtestPvaluePaired >= 0.05, "EQUAL", ifelse(A12estPaired >= 0.638, ifelse(TRUE, qi1, qi2), ifelse(A12estPaired <= 1-0.638, ifelse(TRUE, qi2, qi1), "EQUAL")))
        
        if(!(PreferredPaired=="EQUAL")) {
          row <- data.frame(Algo=alg, QI1=qi1, QI2=qi2, Preferred=PreferredPaired)
          dataStructureBetter <- rbind(dataStructureBetter, row)
        }
      }
    }
  }
}

write.table(dataStructureBetter, file = "results/RQ2.2pvaluesBetter.txt", sep = "\t", quote = FALSE, row.names = FALSE)
