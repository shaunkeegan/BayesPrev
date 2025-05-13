library(dplyr)
source("funcs/identify_epi_data.R")
testdat1 <- read.csv("sample_data.csv") # Insert your own data here

# Automatic Mode
results_automatic <- identify_epi_data(testdat1)
head(results_automatic)

# Semi-manual Mode - Specify numeric columns
results_manual1 <- identify_epi_data(testdat1, counts = c("adj_count", "adj_sum"))
head(results_manual1)

# Semi-manual Mode - Specify grouping columns
results_manual2 <- identify_epi_data(testdat1, groups = c("Site", "Region"))
head(results_manual2)

results_manual3 <- identify_epi_data(testdat1, counts = c("adj_count", "adj_sum"), groups = c("Site", "Region"))
head(results_manual3)
