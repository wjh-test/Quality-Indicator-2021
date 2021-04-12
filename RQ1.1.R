setwd(".")
rm(list = ls(all = TRUE))

data <- read.table(file = "data/inputDataDiffStructure.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")

overallcount <- 0

dataDiffStructure <- data.frame()

for (qi in QIs)
{
  dataQI <- data.frame()
  for (alg in ALGs)
  {
    count <- NROW(subset(data, data$QI==qi & data$Algo==alg))
    den <- NROW(subset(data, data$QI==qi & (data$Alg1==alg | data$Alg2==alg)))
    percen <- count/den
    row <- data.frame(QI=qi, Algo = alg, Counter = count, Percentage=percen, PercentageII=percen*100)
    dataDiffStructure <- rbind(dataDiffStructure, row)
    overallcount <- overallcount+count
  }
  dataQI$id <- seq.int(nrow(dataQI))
  dataQI <- dataQI[,c(1,2,6,3,4,5)]
  write.table(dataQI, file = paste0("results/byQI/overall",qi,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  dataQI <- dataQI[order(-dataQI$Percentage),]
  dataQI$id <- seq.int(nrow(dataQI))
  write.table(dataQI, file = paste0("results/byQI/overallSort",qi,".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
}

write.table(
  dataDiffStructure,
  file = "results/RQ1.1.txt",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

print(overallcount)
