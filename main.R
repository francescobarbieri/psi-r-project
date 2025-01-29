alldata <- read.table("revenues2023.txt", header = FALSE, sep = "\t")
# Removing the first column, defect in parsing txt file
alldata <- alldata[, -1]
vendite <- alldata[, 1:9]
ricavi <- alldata[, c(1, 10:ncol(alldata))]
names(vendite) <- c("settimana", "trekking", "mtb", "strada", "abbigliamento", "accessori", "componenti", "componenti-assistenza", "servizi")
names(ricavi) <- c("settimana", "trekking", "mtb", "strada", "abbigliamento", "accessori", "componenti", "servizi")
str(vendite)
str(ricavi)