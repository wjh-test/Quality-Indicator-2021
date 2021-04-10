setwd(".")
rm(list = ls(all = TRUE))

QIsPreferences <- read.table("./QIsPreferences.csv", head = TRUE, sep = ";")
UncerTest <- read.table("./UncerTest.csv", head = TRUE, sep = ";")
URPS <- read.table("./URPS.csv", head = TRUE, sep = ";")

inputData <- data.frame()

inputData <- rbind(inputData, QIsPreferences)
inputData <- rbind(inputData, UncerTest)
inputData <- rbind(inputData, URPS)

file.create("./inputData.csv")
write.table(
  inputData,
  file = "./inputData.csv",
  sep = ";",
  quote = FALSE,
  row.names = FALSE
)
