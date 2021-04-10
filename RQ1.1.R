setwd(".")
rm(list = ls(all = TRUE))

data <- read.table(file = "data/inputDataDiffStructure.txt", head = TRUE)
QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2", "ND")

overallcount <- 0

dataDiffStructure <- data.frame()

for (qi in QIs)
{
  dataQI <- data.frame()
  for (alg in ALGs)
  {
    count <- NROW(subset(data, data$QI==qi & data$Algo==alg))
    percen <- 0
    if (alg=='CELLDE')
      percen <- (count/70)#(6-1)*(11+4-1)=5*14=70
    else if(alg=='MOCELL'|alg=='NSGA-II'|alg=='PAES'|alg=='SPEA2')
      percen <- (count/83)#(6-1)*(11+4-1)+(5-1)*(1)+(4-1)*(3)=5*14+4*1+3*3=70+4+9=83
    else if (alg == 'ND')
      percen <- (count/238)#(15)*(11+4-1)+(10)*(1)+(6)*(3)=15*14+10*1+6*3=210+10+18=238
    else
      percen <- (count/74)#(6-1)*(11+4-1)+(5-1)*(1)=5*14+4*1=70+4=74
    row <- data.frame(QI=qi, Algo = alg, Counter = count, Percentage=percen, PercentageII=percen*100)
    dataDiffStructure <- rbind(dataDiffStructure, row)
    if(alg!="ND") {
      dataQI <- rbind(dataQI, row)
    }
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
