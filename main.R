alldata <- read.table("revenues2023.txt", header = FALSE, sep = "\t")
# Removing the first column, defect in parsing txt file
alldata <- alldata[, -1]
vendite <- alldata[, 1:9]
ricavi <- alldata[, c(1, 10:ncol(alldata))]

# Assign correct column names
names(vendite) <- c("settimana", "trekking", "mtb", "strada", "abbigliamento", "accessori", "componenti", "componenti-assistenza", "servizi")
names(ricavi) <- c("settimana", "trekking", "mtb", "strada", "abbigliamento", "accessori", "componenti", "servizi")

# Convert characters columns with comma as decimal separator to numeric
ricavi[] <- lapply(ricavi, function(x) {
  if (is.character(x)) as.numeric(gsub(",", ".", x)) else x
})

# Check structure after conversion
str(vendite)
str(ricavi)

# For 'vendite'
# List of columns to analyze
cols_to_analyze <- c("trekking", "mtb", "strada", "abbigliamento", "accessori", "componenti", "componenti-assistenza", "servizi")
vendite_summary <- data.frame(
  column = cols_to_analyze,
  mean = sapply(cols_to_analyze, function(x) mean(vendite[[x]], na.rm = TRUE)),
  median = sapply(cols_to_analyze, function(x) median(vendite[[x]], na.rm = TRUE)),
  min = sapply(cols_to_analyze, function(x) min(vendite[[x]], na.rm = TRUE)),
  max = sapply(cols_to_analyze, function(x) max(vendite[[x]], na.rm = TRUE)),
  sd = sapply(cols_to_analyze, function(x) sd(vendite[[x]], na.rm = TRUE))
)
# For 'ricavi'
# Note: 'componenti-assistenza' is not in ricavi, so we exclude it
cols_to_analyze_ricavi <- setdiff(cols_to_analyze, "componenti-assistenza")
ricavi_summary <- data.frame(
  column = cols_to_analyze_ricavi,
  mean = sapply(cols_to_analyze_ricavi, function(x) mean(ricavi[[x]], na.rm = TRUE)),
  median = sapply(cols_to_analyze_ricavi, function(x) median(ricavi[[x]], na.rm = TRUE)),
  min = sapply(cols_to_analyze_ricavi, function(x) min(ricavi[[x]], na.rm = TRUE)),
  max = sapply(cols_to_analyze_ricavi, function(x) max(ricavi[[x]], na.rm = TRUE)),
  sd = sapply(cols_to_analyze_ricavi, function(x) sd(ricavi[[x]], na.rm = TRUE))
)

# Display summaries
print(vendite_summary)
print(ricavi_summary)

# Creating 2 variables
vendite_totali <- rowSums(alldata[, 2:9])
ricavi_totali <- rowSums(ricavi[, 2:8])

barplot(vendite_totali)
barplot(ricavi_totali)

# Correlazione 1
cor(vendite$servizi, vendite$componenti, use = "complete.obs")

# Correlazione 2
cor(rowSums(vendite [, 2:4]), vendite$accessori, use = "complete.obs")

# Correlazione 3
cor(ricavi$servizi, ricavi$componenti, use = "complete.obs")

# Plotting correlation idexes - dispersion plots
plot(vendite$servizi, vendite$componenti, 
     main = "Vendite: Servizi vs Componenti",
     xlab = "Vendite Servizi", 
     ylab = "Vendite Componenti", 
     pch = 16)

plot(rowSums(vendite[, 2:4]), vendite$accessori, 
     main = "Totale Trekking, MTB, Strada vs Vendite Accessori",
     xlab = "Totale Trekking + MTB + Strada", 
     ylab = "Vendite Accessori", 
     pch = 16)

plot(ricavi$servizi, ricavi$componenti, 
     main = "Ricavi: Servizi vs Componenti",
     xlab = "Ricavi Servizi", 
     ylab = "Ricavi Componenti", 
     pch = 16)
