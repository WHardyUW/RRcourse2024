# Set the working directory
setwd("C:\\Users\\mraer\\Desktop\\UW\\Semester 4\\Reproducible Research\\RRcourse2024")

# Import required libraries
library(readxl)
library(dplyr)
library(Hmisc)

# Function to calculate weighted standard deviation
weighted_sd <- function(x, w) {
  sqrt(wtd.var(x, w))
}

# Function to standardize task values
standardize_task_values <- function(task_column, share_column) {
  temp_mean <- wtd.mean(task_column, share_column)
  temp_sd <- weighted_sd(task_column, share_column)
  return((task_column - temp_mean) / temp_sd)
}

# Function to calculate country-level aggregate
country_level_aggregate <- function(data, value_column, weight_column, group_column) {
  agg_data <- aggregate(data[[value_column]] * data[[weight_column]], by = list(data[[group_column]]), FUN = sum, na.rm = TRUE)
  names(agg_data)[1] <- "Time"
  return(agg_data)
}

# Read task data
task_data <- read.csv("Data\\onet_tasks.csv")
task_data$isco08_1dig <- as.numeric(substr(task_data$isco08, 1, 1))

# Aggregate task data
agg_task_data <- aggregate(task_data[, 5:ncol(task_data)], by = list(task_data$isco08_1dig), FUN = mean, na.rm = TRUE)

# Read employment data from Eurostat
eurostat_data <- lapply(1:9, function(i) read_excel("Data\\Eurostat_employment_isco.xlsx", sheet = paste0("ISCO", i)))

# Combine all employment data
all_data <- do.call(rbind, eurostat_data)
all_data$ISCO <- rep(1:9, each = nrow(eurostat_data))

# Calculate total workers for each country
total_workers <- sapply(c("Belgium", "Spain", "Poland"), function(country) {
  rowSums(sapply(eurostat_data, `[[`, country))
})

# Add total workers to all_data
all_data <- cbind(all_data, total_workers)

# Calculate shares of workers for each country
for (country in c("Belgium", "Spain", "Poland")) {
  total_country <- all_data[[paste0("total_", country)]]
  all_data[[paste0("share_", country)]] <- ifelse(total_country == 0, 0, all_data[[country]] / total_country)
}

# Join all_data with aggregated task data
combined <- left_join(all_data, agg_task_data, by = c("ISCO" = "Group.1"))

# Standardize task values for each country
task_columns <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")
for (country in c("Belgium", "Spain", "Poland")) {
  for (task in task_columns) {
    combined[[paste0("std_", country, "_", task)]] <- standardize_task_values(combined[[task]], combined[[paste0("share_", country)]])
  }
}

# Calculate non-routine cognitive analytical tasks
for (country in c("Belgium", "Spain", "Poland")) {
  combined[[paste0(country, "_NRCA")]] <- rowSums(select(combined, starts_with(paste0("std_", country, "_"))))
}

# Standardize NRCA
for (country in c("Belgium", "Spain", "Poland")) {
  combined[[paste0("std_", country, "_NRCA")]] <- standardize_task_values(combined[[paste0(country, "_NRCA")]], combined[[paste0("share_", country)]])
}

# Calculate country-level mean
agg_Belgium <- country_level_aggregate(combined, paste0("std_Belgium_NRCA"), paste0("share_Belgium"), "TIME")
agg_Spain <- country_level_aggregate(combined, paste0("std_Spain_NRCA"), paste0("share_Spain"), "TIME")
agg_Poland <- country_level_aggregate(combined, paste0("std_Poland_NRCA"), paste0("share_Poland"), "TIME")

# Plot data
par(mfrow = c(3, 1))
plot(agg_Belgium$Time, agg_Belgium$x, xaxt = "n", main = "Belgium")
axis(1, at = seq(1, 40, 3), labels = agg_Belgium$Time[seq(1, 40, 3)])

plot(agg_Spain$Time, agg_Spain$x, xaxt = "n", main = "Spain")
axis(1, at = seq(1, 40, 3), labels = agg_Spain$Time[seq(1, 40, 3)])

plot(agg_Poland$Time, agg_Poland$x, xaxt = "n", main = "Poland")
axis(1, at = seq(1, 40, 3), labels = agg_Poland$Time[seq(1, 40, 3)])
