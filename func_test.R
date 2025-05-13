library(dplyr)
testdat1 <- read.csv("testdat1.csv")
testdat2 <- testdat1 %>% select(raw_count, raw_sum)
identify_epi_data(testdat2)

testdat2 <- tibble(
  raw_count = c(210, 49, 40, 39, 35, 26, 93, 40, 20, 21),
  raw_sum = c(325, 168, 325, 168, 325, 168, 254, 254, 254, 30),
  new_count = c(211, 50, 41, 34, 37, 29, 95, 44, 22, 28),
  new_sum = c(326, 167, 326, 169, 326, 169, 255, 257, 254, 35)
)

result <- identify_epi_data(testdat2)
result <- identify_epi_data(testdat2, counts = c("new_count", "new_sum"))

result <- identify_epi_data(testdat2)
# View the result
result



# Generate sample epidemiological data with grouping variables
set.seed(123)
testdat3 <- tibble(
  cases_A = sample(10:50, 20, replace = TRUE),
  total_A = sample(100:500, 20, replace = TRUE),
  cases_B = sample(5:30, 20, replace = TRUE),
  total_B = sample(50:300, 20, replace = TRUE),
  region = rep(c("North", "South", "East", "West"), each = 5),
  site = rep(paste("Site", LETTERS[1:5]), 4),
  year = rep(2020:2023, each = 5),
  quality_flag = sample(c("Good", "Fair", "Poor"), 20, replace = TRUE)
)


# Automatic detection with grouping variables
result_auto <- identify_epi_data(testdat3)
print(result_auto)

# Manual specification with grouping variables
result_manual <- identify_epi_data(testdat3, counts = c("cases_B", "total_B"))
print(result_manual)



# Full automatic mode
result_auto <- identify_epi_data(testdat3)

# Manual count columns with auto-detected groups
result_semi <- identify_epi_data(testdat3, counts = c("cases_B", "total_B"))

# Fully manual specification
result_manual <- identify_epi_data(testdat3, 
                                   counts = c("cases_A", "total_A"),
                                   groups = c("region", "year"))

# Manual groups only (auto-detect counts)
result_groups <- identify_epi_data(testdat3, groups = "site")


