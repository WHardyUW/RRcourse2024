# ---------------------  Path
setwd("C:\\Users\\nomin\\RRcourse2024") # sets path where you want to work

# --------------------- Packages
library(readxl) # this package helps us to read excel files

# --------------------- Data import
task_data = read.csv("Data\\onet_tasks.csv") # import excel file


# --------------------- Data prepareting
isco_list <- list() # creating list

sheet_names <- c("ISCO1", "ISCO2", "ISCO3", "ISCO4", "ISCO5", "ISCO6", "ISCO7", "ISCO8", "ISCO9") # list of sheet names

for (sheet_name in sheet_names) {
  isco_list[[sheet_name]] <- read_excel("Data\\Eurostat_employment_isco.xlsx", sheet = sheet_name) # loop for read excel using previous sheet names
  isco_list[[sheet_name]]$ISCO <- as.numeric(substr(sheet_name, 5, 5))
}

# ---------------------  Merge all datasets
all_data <- do.call(rbind, isco_list) # merged datasets

# --------------------- Applying Belgium, Spain, and Poland
total_Belgium <- rowSums(do.call(cbind, lapply(isco_list, `[[`, "Belgium")))
total_Spain <- rowSums(do.call(cbind, lapply(isco_list, `[[`, "Spain")))
total_Poland <- rowSums(do.call(cbind, lapply(isco_list, `[[`, "Poland")))

# We have 9 occupations and the same time range for each, so we an add the totals by
# adding a vector that is 9 times the previously calculated totals
all_data$total_Belgium <- rep(total_Belgium, length(unique(all_data$ISCO)))
all_data$total_Spain <- rep(total_Spain, length(unique(all_data$ISCO)))
all_data$total_Poland <- rep(total_Poland, length(unique(all_data$ISCO)))

# And this will give us shares of each occupation among all workers in a period-country
all_data$share_Belgium <- all_data$Belgium / all_data$total_Belgium
all_data$share_Spain <- all_data$Spain / all_data$total_Spain
all_data$share_Poland <- all_data$Poland / all_data$total_Poland

# Now let's look at the task data. We want the first digit of the ISCO variable only
library(stringr)

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <-aggregate(task_data, by=list(task_data$isco08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

#Let's combine the data.
library(dplyr)

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

#install.packages("Hmisc")
library(Hmisc)

# Function to calculate standardized variable
calculate_standardized <- function(data, column, share_column) {
  temp_mean <- wtd.mean(data[[column]], data[[share_column]])
  temp_sd <- sqrt(wtd.var(data[[column]], data[[share_column]]))
  return((data[[column]] - temp_mean) / temp_sd)
}

# First task item
combined$std_Belgium_t_4A2a4 <- calculate_standardized(combined, "t_4A2a4", "share_Belgium")
combined$std_Poland_t_4A2a4 <- calculate_standardized(combined, "t_4A2a4", "share_Poland")
combined$std_Spain_t_4A2a4 <- calculate_standardized(combined, "t_4A2a4", "share_Spain")

# Second task item
combined$std_Belgium_t_4A2b2 <- calculate_standardized(combined, "t_4A2b2", "share_Belgium")
combined$std_Poland_t_4A2b2 <- calculate_standardized(combined, "t_4A2b2", "share_Poland")
combined$std_Spain_t_4A2b2 <- calculate_standardized(combined, "t_4A2b2", "share_Spain")

# Third task item
combined$std_Belgium_t_4A4a1 <- calculate_standardized(combined, "t_4A4a1", "share_Belgium")
combined$std_Poland_t_4A4a1 <- calculate_standardized(combined, "t_4A4a1", "share_Poland")
combined$std_Spain_t_4A4a1 <- calculate_standardized(combined, "t_4A4a1", "share_Spain")


# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

# Function to calculate standardized NRCA variable
calculate_standardized_NRCA <- function(data, columns, share_column) {
  NRCA <- rowSums(data[columns])
  temp_mean <- wtd.mean(NRCA, data[[share_column]])
  temp_sd <- sqrt(wtd.var(NRCA, data[[share_column]]))
  return((NRCA - temp_mean) / temp_sd)
}

# Calculate standardized NRCA variables for each country
combined$Belgium_NRCA <- combined$std_Belgium_t_4A2a4 + combined$std_Belgium_t_4A2b2 + combined$std_Belgium_t_4A4a1
combined$Poland_NRCA <- combined$std_Poland_t_4A2a4 + combined$std_Poland_t_4A2b2 + combined$std_Poland_t_4A4a1
combined$Spain_NRCA <- combined$std_Spain_t_4A2a4 + combined$std_Spain_t_4A2b2 + combined$std_Spain_t_4A4a1

combined$std_Belgium_NRCA <- calculate_standardized_NRCA(combined, c("std_Belgium_t_4A2a4", "std_Belgium_t_4A2b2", "std_Belgium_t_4A4a1"), "share_Belgium")
combined$std_Poland_NRCA <- calculate_standardized_NRCA(combined, c("std_Poland_t_4A2a4", "std_Poland_t_4A2b2", "std_Poland_t_4A4a1"), "share_Poland")
combined$std_Spain_NRCA <- calculate_standardized_NRCA(combined, c("std_Spain_t_4A2a4", "std_Spain_t_4A2b2", "std_Spain_t_4A4a1"), "share_Spain")


# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
combined$multip_Spain_NRCA <- (combined$std_Spain_NRCA*combined$share_Spain)
combined$multip_Belgium_NRCA <- (combined$std_Belgium_NRCA*combined$share_Belgium)
combined$multip_Poland_NRCA <- (combined$std_Poland_NRCA*combined$share_Poland)

# Step 2: sum it up (it basically becomes another weighted mean)
agg_Spain <-aggregate(combined$multip_Spain_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)
agg_Belgium <-aggregate(combined$multip_Belgium_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)
agg_Poland <-aggregate(combined$multip_Poland_NRCA, by=list(combined$TIME),
                      FUN=sum, na.rm=TRUE)

# We can plot it now!
plot(agg_Poland$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Poland$Group.1[seq(1, 40, 3)])

plot(agg_Spain$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Spain$Group.1[seq(1, 40, 3)])

plot(agg_Belgium$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Belgium$Group.1[seq(1, 40, 3)])

