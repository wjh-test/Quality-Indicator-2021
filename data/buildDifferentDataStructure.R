setwd(".")
rm(list = ls(all = TRUE))
library(stringr)

data <- read.table(file = "./inputData.csv", head = TRUE, sep = ";")
data$A <- str_replace(data$A, "NSGAII", "NSGA-II")
data$B <- str_replace(data$B, "NSGAII", "NSGA-II")

QIs <- c("HV", "IGD", "EP", "GD", "GS", "ED", "PFS", "C")
ALGs <- c("CELLDE", "MOCELL", "NSGA-II", "PAES", "SMPSO", "SPEA2")

data$CaseStudy <- as.character(data$CaseStudy)
#TP1 remains TP1
#TP2, TP3 and TP4 should become TP2_1, TP2_2, TP2_3
data$CaseStudy[data$CaseStudy == "TP2"] <- "TP2_1"
data$CaseStudy[data$CaseStudy == "TP3"] <- "TP2_2"
data$CaseStudy[data$CaseStudy == "TP4"] <- "TP2_3"
data$CaseStudy <- as.factor(data$CaseStudy)

#Problems <- c("RA", "TS", "TRA", "RP", "TM", "TP1", "TP2", "TP3", "TP4", "RM", "ITO", ...)
Problems <- as.vector(unique(data$CaseStudy))

dataDiffStructure <- data.frame()
for (i in 1:length(data$HV))
{
  for (j in 4:11)
  {
    val = data[i,j]
    if (val=='A')
    {
      val2=data$A[i]
    }
    else if (val=='B')
      val2=data$B[i]
    else
      val2 ="ND"
    #  val2=paste(data$A[i], data$B[i], sep="_")
    row <- data.frame(Problem = data$CaseStudy[i], QI=QIs[j-3], Algo = val2, Alg1=data$A[i], Alg2=data$B[i])
    dataDiffStructure <- rbind(dataDiffStructure, row)
  }
}
write.table(
  dataDiffStructure,
  file = "./inputDataDiffStructure.txt",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

dataByPair <- data.frame()
for (qi in QIs)
{
  dataByPairAndQI <- data.frame()
  dataQI <-subset(dataDiffStructure, dataDiffStructure$QI==qi)
  count <- 1
  for (alg1 in ALGs)
  {
    for (alg2 in ALGs) {
      if(alg1 != alg2) {
        dataALGs <-subset(dataQI, dataQI$Alg1==alg1 & dataQI$Alg2==alg2)
        if(NROW(dataALGs)>0) {
          if(alg1=="CELLDE") {
            firstSelden <- 10+4#14
          }
          else if(alg1=="MOCELL"|alg1=="NSGA-II"|alg1=="PAES"|alg1=="SPEA2") {
            firstSelden <- 11+4+3#18
          }
          else {
            firstSelden <- 11+4#15
          }
          if(alg2=="CELLDE") {
            secondSelden <- 10+4#14
          }
          else if(alg2=="MOCELL"|alg2=="NSGA-II"|alg2=="PAES"|alg2=="SPEA2") {
            secondSelden <- 11+4+3#18
          }
          else {
            secondSelden <- 11+4#15
          }
          firstSel <- NROW(subset(dataALGs, dataALGs$Algo==alg1))
          secondSel <- NROW(subset(dataALGs, dataALGs$Algo==alg2))
          firstSelPerc <- firstSel/firstSelden
          secondSelPerc <- secondSel/secondSelden
          id <- paste0(alg1, "_", alg2)
          row <- data.frame(QI=qi, IDcount=count, ID=id, Algo1 = alg1, Algo2 = alg2, FirstSel = firstSel, SecondSel = secondSel, FirstSelPerc = firstSelPerc, SecondSelPerc = secondSelPerc)
          dataByPairAndQI <- rbind(dataByPairAndQI, row)
          count <- count + 1
        }
      }
    }
  }
  write.table(
    dataByPairAndQI,
    file = paste0("./dataByPair", qi, ".txt"),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  dataByPair <- rbind(dataByPair, dataByPairAndQI)
}
write.table(
  dataByPair,
  file = "./dataByPair.txt",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)