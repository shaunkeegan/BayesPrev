library(dplyr)
source("funcs/identify_epi_data.R")
testdat1 <- read.csv("sample_data.csv") # Insert your own data here

# Automatic Mode -
# This mode will scan the data for appropriate integer columns, and assess what 
# possible pairs of cases/totals exist. If only one possible pair exists, it is
# selected. If multiples exist, the user will be asked to select their preferred
# choice. It will then scan for factor columns which can be added to the dataset
# as grouping variables, allowing the user to select 0 - n of these.

results_automatic <- identify_epi_data(testdat1)
head(results_automatic)

# Semi-manual Mode - Specify numeric columns
# The user specifies their desired pair of numeric columns to be included in the
# output data. It will then scan for factor columns which can be added to the 
# dataset as grouping variables, allowing the user to select 0 - n of these.

results_manual1 <- identify_epi_data(testdat1, counts = c("adj_count", "adj_sum"))
head(results_manual1)

# Semi-manual Mode - Specify grouping columns
# The user specifies their desired grouping columns to be included in the output 
# data. It will then scan the data for appropriate integer columns, and assess 
# what possible pairs of cases/totals exist. If only one possible pair exists, 
# it is selected. If multiples exist, the user will be asked to select their 
# preferred choice.  

results_manual2 <- identify_epi_data(testdat1, groups = c("Site", "Region"))
head(results_manual2)

# Manual Mode
# The user specifies their desired pair of numeric columns to be included in the
# output data. # The user specifies their desired grouping columns to be 
# included in the output data. 

results_manual3 <- identify_epi_data(testdat1, counts = c("adj_count", "adj_sum"), groups = c("Site", "Region"))
head(results_manual3)
