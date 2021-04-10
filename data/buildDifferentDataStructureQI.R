setwd(".")
rm(list = ls(all = TRUE))
library(stringr)

data <- read.table(file = "inputDataDiffStructure.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")
#Problems <- c("RA", "TS", "TRA", "RP", "TM", "TP1", "TP2", "TP3", "TP4", "RM", "ITO", ...)
Problems <- as.vector(unique(data$Problem))

dataDiffStructure <- data.frame()
for(p in Problems)
{
  for(qi1 in QIs)
  {
    for(qi2 in QIs[(which(QIs==qi1)+1):length(QIs)])
    {
      for(alg in ALGs)
      {
        count1 <- NROW(subset(data, data$Problem==p & data$QI==qi1 & data$Algo==alg))
        count2 <- NROW(subset(data, data$Problem==p & data$QI==qi2 & data$Algo==alg))
        if(count1 > count2) {
          QI <- qi1
        } else if(count1 == count2) {
          QI <- "ND"
        } else {
          QI <- qi2
        }
        row <- data.frame(Problem = p, Algo = alg, QI = QI, QI1 = qi1, QI2 = qi2)
        dataDiffStructure <- rbind(dataDiffStructure, row)
      }
    }
    if(qi1==QIs[length(QIs)-1])
      break
  }
}

write.table(
  dataDiffStructure,
  file = "./inputDataDiffStructureQI.txt",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)