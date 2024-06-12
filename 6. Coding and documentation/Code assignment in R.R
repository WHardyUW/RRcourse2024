
# Set working directory
setwd("Z:\\File folders\\Teaching\\Reproducible Research\\2023\\Repository\\RRcourse2023\\6. Coding and documentation")

library(readxl)
library(dplyr)
library(Hmisc)


# Read task data
task_data <- read.csv("Data\\onet_tasks.csv")

# Function to read and aggregate employment data by country
read_and_aggregate_data <- function(file_path) {
  sheets <- paste0("ISCO", 1:9)
  isco_data <- lapply(sheets, function(sheet) read_excel(file_path, sheet = sheet))
  total_employment <- sapply(isco_data, function(df) rowSums(select(df, -TIME)))
  return(total_employment)
}

# Function to calculate standardized task values by country
calculate_standardized_values <- function(data, task_column, share_column) {
  temp_mean <- wtd.mean(data[[task_column]], data[[share_column]])
  temp_sd <- sqrt(wtd.var(data[[task_column]], data[[share_column]]))
  return((data[[task_column]] - temp_mean) / temp_sd)
}

# Function to perform aggregation and standardization for a specific task item
aggregate_and_standardize_task <- function(combined_data, task_column, share_column, country_prefix) {
  combined_data[[paste0("std_", country_prefix, "_", task_column)]] <-
    calculate_standardized_values(combined_data, task_column, share_column)
}



# Read employment data and calculate totals by country
employment_file <- "Data\\Eurostat_employment_isco.xlsx"
total_employment <- read_and_aggregate_data(employment_file)

# Combine employment data into one dataframe
all_data <- bind_rows(total_employment, .id = "ISCO")

# Merge task data with employment data based on ISCO code
aggdata <- task_data %>%
  mutate(isco08_1dig = as.numeric(substr(isco08, 1, 1))) %>%
  group_by(isco08_1dig) %>%
  summarise(across(starts_with("t_"), mean, na.rm = TRUE))

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Perform aggregation and standardization for task items by country
task_items <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")
countries <- c("Belgium", "Poland", "Spain")

for (item in task_items) {
  for (country in countries) {
    aggregate_and_standardize_task(combined, item, paste0("share_", country), country)
  }
}

# Calculate NRCA and its standardized values by country
for (country in countries) {
  combined[[paste0(country, "_NRCA")]] <- rowSums(select(combined, starts_with(paste0("std_", country))))
  combined[[paste0("std_", country, "_NRCA")]] <-
    calculate_standardized_values(combined, paste0(country, "_NRCA"), paste0("share_", country))
}

# Aggregate NRCA values by country over time
nrca_aggregates <- lapply(countries, function(country) {
  aggregate(combined[[paste0("std_", country, "_NRCA")]] * combined[[paste0("share_", country)]],
            by = list(combined$TIME), FUN = sum, na.rm = TRUE
  )
})

# Plot NRCA values for each country
par(mfrow = c(3, 1)) # Set up multiple plots in one window

for (i in seq_along(countries)) {
  plot(nrca_aggregates[[i]]$x, xaxt = "n", main = countries[i])
  axis(1, at = seq(1, 40, 3), labels = nrca_aggregates[[i]]$Group.1[seq(1, 40, 3)])
}
