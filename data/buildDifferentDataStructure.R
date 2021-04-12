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
