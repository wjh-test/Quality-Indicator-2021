setwd(".")
rm(list = ls(all = TRUE))
library("effsize")#for A12 test https://rdrr.io/cran/effsize/man/VD.A.html

effectSize <- function(UtestPvalue, A12est, Category=""){
  if(UtestPvalue < 0.05){
    if((A12est >= 0.556 && A12est < 0.638)||(A12est <= 1-0.556 && A12est > 1-0.638))
      Category <- "small"
    else if((A12est >= 0.638 && A12est < 0.714)||(A12est <= 1-0.638 && A12est > 1-0.714))
      Category <- "medium"
    else if((A12est >= 0.714 && A12est <= 1.0)||(A12est <= 1-0.714 && A12est >= 0.0))
      Category <- "large"
    else
      Category <- "negligible"
  } else Category <- "negligible"
}

statTest <- function(data1, data2, QI1, QI2, greaterBetter, alg, data1AllResults, data2AllResults) {
  #null hypothesis is that the populations are the same
  #if p-value is less than 0.05, we can reject the null hypothesis
  UtestPvalueUnpaired <- wilcox.test(data1, data2, exact = FALSE, paired = FALSE)$p.value
  A12estUnpaired <- VD.A(data1, data2, paired = FALSE)$estimate #A12
  PreferredUnpaired <- ifelse(UtestPvalueUnpaired >= 0.05, "EQUAL", ifelse(A12estUnpaired > 0.5, ifelse(greaterBetter, QI1, QI2), ifelse(A12estUnpaired < 0.5, ifelse(greaterBetter, QI2, QI1), "EQUAL")))
  
  UtestPvaluePaired <- wilcox.test(data1, data2, exact = FALSE, paired = TRUE)$p.value
  A12estPaired <- VD.A(data1, data2, paired = TRUE)$estimate #A12
  PreferredPaired <- ifelse(UtestPvaluePaired >= 0.05, "EQUAL", ifelse(A12estPaired > 0.5, ifelse(greaterBetter, QI1, QI2), ifelse(A12estUnpaired < 0.5, ifelse(greaterBetter, QI2, QI1), "EQUAL")))
  Category <- effectSize(UtestPvaluePaired, A12estPaired)
  
  UtestPvalueUnpairedAll <- wilcox.test(data1AllResults, data2AllResults, exact = FALSE, paired = FALSE)$p.value
  A12estUnpairedAll <- VD.A(data1AllResults, data2AllResults, paired = FALSE)$estimate #A12
  PreferredUnpairedAll <- ifelse(UtestPvalueUnpairedAll >= 0.05, "EQUAL", ifelse(A12estUnpairedAll > 0.5, ifelse(greaterBetter, QI1, QI2), ifelse(A12estUnpaired < 0.5, ifelse(greaterBetter, QI2, QI1), "EQUAL")))
  
  row <- data.frame(alg, QI1, QI2, Category,
                    PreferredUnpaired, PreferredPaired, PreferredUnpairedAll,
                    UtestPvalueUnpaired, A12estUnpaired, UtestPvaluePaired, A12estPaired, UtestPvalueUnpairedAll, A12estUnpairedAll)
}

data <- read.table(file = "data/inputDataDiffStructureQI.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")
#Problems <- c("RA", "TS", "TRA", "RP", "TM", "TP1", "TP2_1", "TP2_2", "TP2_3", "RM", "ITO", ...)
Problems <- as.vector(unique(data$Problem))


dataS <- read.table(file = "results/RQ2.2.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
dataStructureBetterPairedUnpaired <- data.frame()#only strictly better
dataStructureBetterEqPairedUnpaired <- data.frame()#better and equal
dataStructureBetterPaired <- data.frame()#only strictly better
diffPairedUnpaired <- data.frame()
dataStructureBetterEffectSize <- data.frame()
dataStructureBetterNegligible <- data.frame()
dataStructureBetterSmall <- data.frame()
dataStructureBetterMedium <- data.frame()
dataStructureBetterLarge <- data.frame()
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
        
        row <- statTest(q1dataSel$Percentage, q2dataSel$Percentage, qi1, qi2, TRUE, alg, q1data$Percentage, q2data$Percentage)
        
        dataStructureBetterEqPairedUnpaired <- rbind(dataStructureBetterEqPairedUnpaired, row)
        if(!(row$PreferredPaired=="EQUAL")) {
          dataStructureBetterPairedUnpaired <- rbind(dataStructureBetterPairedUnpaired, row)
          rowFiltered <- data.frame(Algo=row$alg, QI1=row$QI1, QI2=row$QI2, Preferred=row$PreferredPaired)
          dataStructureBetterPaired <- rbind(dataStructureBetterPaired, rowFiltered)
          rowEffectSize <- data.frame(Algo=row$alg, QI1=row$QI1, QI2=row$QI2, Preferred=row$PreferredPaired, EffectValue=row$A12estPaired, Category=row$Category)
          dataStructureBetterEffectSize <- rbind(dataStructureBetterEffectSize, rowEffectSize)
          if(row$Category=="small")
            dataStructureBetterSmall <- rbind(dataStructureBetterSmall, rowEffectSize)
          else if(row$Category=="medium")
            dataStructureBetterMedium <- rbind(dataStructureBetterMedium, rowEffectSize)
          else if(row$Category=="large")
            dataStructureBetterLarge <- rbind(dataStructureBetterLarge, rowEffectSize)
          else
            dataStructureBetterNegligible <- rbind(dataStructureBetterNegligible, rowEffectSize)
        }
        if(row$PreferredUnpairedAll!=row$PreferredPaired) {
          rowFilteredDiff <- data.frame(Algo=row$alg, QI1=row$QI1, QI2=row$QI2, PreferredPaired=row$PreferredPaired, PreferredUnpaired=row$PreferredUnpairedAll)
          diffPairedUnpaired <- rbind(diffPairedUnpaired, rowFilteredDiff)
        }
      }
    }
  }
}

write.table(dataStructureBetterPaired, file = "results/RQ2.2pvaluesBetter_Paired.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(diffPairedUnpaired, file = "results/RQ2.2_diff_pair_unpaired.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetterEffectSize, file = "results/RQ2.2pvaluesBetter_EffectSize.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetterSmall, file = "results/RQ2.2pvaluesBetter_EffectSize_Small.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetterMedium, file = "results/RQ2.2pvaluesBetter_EffectSize_Medium.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetterLarge, file = "results/RQ2.2pvaluesBetter_EffectSize_Large.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.table(dataStructureBetterNegligible, file = "results/RQ2.2pvaluesBetter_EffectSize_Negligible.txt", sep = "\t", quote = FALSE, row.names = FALSE)
