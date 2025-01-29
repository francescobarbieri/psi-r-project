data <- read.table("revenues2023.txt", header=FALSE, sep="\t")
vendite <- data[, 1:9]
ricavi <- data[, 10:ncol(data)]