setwd(".")
rm(list = ls(all = TRUE))

data <- read.table(file = "data/inputDataDiffStructureQI.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C", "ND")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")

overallcount <- 0

dataDiffStructure <- data.frame()

for (alg in ALGs)
{
  dataALG <- data.frame()
  for (qi in QIs)
  {
    count <- NROW(subset(data, data$Algo==alg & data$QI==qi))
    countQI <- (8-1) # the number of QIs
    if (qi=='ND')
    {
      countQI <- 28#1+2+3+4+5+6+7=28
    }
    countP <- 0 # the number of Problems
    if (alg=='CELLDE')
      countP <- (11+4-1)
    else if(alg=='MOCELL'|alg=='NSGA-II'|alg=='PAES'|alg=='SPEA2')
      countP <- (11+4+3)
    else
      countP <- (11+4)
    percen <- count/(countQI*countP)
    row <- data.frame(Algo = alg, QI=qi, Counter = count, Percentage=percen, PercentageII=percen*100)
    dataDiffStructure <- rbind(dataDiffStructure, row)
    if(qi!="ND") {
      dataALG <- rbind(dataALG, row)
    }
    overallcount <- overallcount+count
  }
  dataALG$id <- seq.int(nrow(dataALG))
  dataALG <- dataALG[,c(1,2,6,3,4,5)]
  write.table(dataALG, file = paste0("results/byALG/overall",alg,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  dataALG <- dataALG[order(-dataALG$Percentage),]
  dataALG$id <- seq.int(nrow(dataALG))
  write.table(dataALG, file = paste0("results/byALG/overallSort",alg,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
}

write.table(
  dataDiffStructure,
  file = "results/RQ2.1.txt",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

print(overallcount)
