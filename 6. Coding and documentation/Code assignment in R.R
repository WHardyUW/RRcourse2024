# Sets the path to the parent directory of RR classes
setwd("/Users/komputer/Desktop/RR_git1/RRcourse2024")


# install required packages
library(remotes)
remotes::install_github("r-lib/styler")
library(styler)
library(readxl)

# Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/
# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.
task_data <- read.csv("Data/onet_tasks.csv")



# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job
# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)
sheet_names <- c("ISCO1", "ISCO2", "ISCO3", "ISCO4", "ISCO5", "ISCO6", "ISCO7", "ISCO8", "ISCO9")

# Create empty list to store data frames
isco_data <- list()

# Loop through sheet names and read Excel sheets
for (sheet_name in sheet_names) {
  # Dynamically create variable name
  var_name <- paste0("isco", gsub("ISCO", "", sheet_name))
  # Read Excel sheet and assign to variable
  assign(var_name, read_excel("Data/Eurostat_employment_isco.xlsx", sheet = sheet_name))
  # Add data frame to the list
  isco_data[[var_name]] <- get(var_name)
}


# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?
# This will calculate worker totals in each of the chosen countries.
read_all_isco_sheets <- function(file_path) {
  isco_data <- data.frame()
  for (i in 1:9) {
    isco_sheet <- read_excel(file_path, sheet = paste0("ISCO", i))
    isco_sheet$ISCO <- i
    isco_data <- rbind(isco_data, isco_sheet)
  }
  return(isco_data)
}

# Read all ISCO sheets
all_data <- read_all_isco_sheets("Data/Eurostat_employment_isco.xlsx")

# Calculate totals for each country
total_countries <- colSums(all_data[, 2:ncol(all_data)])

# Add totals to the dataset
all_data$total_Belgium <- total_countries["Belgium"]
all_data$total_Spain <- total_countries["Spain"]
all_data$total_Poland <- total_countries["Poland"]

# Calculate shares for each country
all_data$share_Belgium <- all_data$Belgium / all_data$total_Belgium
all_data$share_Spain <- all_data$Spain / all_data$total_Spain
all_data$share_Poland <- all_data$Poland / all_data$total_Poland
# Now let's look at the task data. We want the first digit of the ISCO variable only
library(stringr)

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level
# (more on what these tasks are below)

aggdata <- aggregate(task_data,
  by = list(task_data$isco08_1dig),
  FUN = mean, na.rm = TRUE
)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

# These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

# Let's combine the data.
library(dplyr)

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

# install.packages("Hmisc")
library(Hmisc)

# first task item
temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_Belgium)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_t_4A2a4 <- (combined$t_4A2a4 - temp_mean) / temp_sd

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_Poland)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_Poland) %>% sqrt()
combined$std_Poland_t_4A2a4 <- (combined$t_4A2a4 - temp_mean) / temp_sd

temp_mean <- wtd.mean(combined$t_4A2a4, combined$share_Spain)
temp_sd <- wtd.var(combined$t_4A2a4, combined$share_Spain) %>% sqrt()
combined$std_Spain_t_4A2a4 <- (combined$t_4A2a4 - temp_mean) / temp_sd

# second task item
temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_Belgium)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_t_4A2b2 <- (combined$t_4A2b2 - temp_mean) / temp_sd

temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_Poland)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_Poland) %>% sqrt()
combined$std_Poland_t_4A2b2 <- (combined$t_4A2b2 - temp_mean) / temp_sd

temp_mean <- wtd.mean(combined$t_4A2b2, combined$share_Spain)
temp_sd <- wtd.var(combined$t_4A2b2, combined$share_Spain) %>% sqrt()
combined$std_Spain_t_4A2b2 <- (combined$t_4A2b2 - temp_mean) / temp_sd

# third task item
temp_mean <- wtd.mean(combined$t_4A4a1, combined$share_Belgium)
temp_sd <- wtd.var(combined$t_4A4a1, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_t_4A4a1 <- (combined$t_4A4a1 - temp_mean) / temp_sd

temp_mean <- wtd.mean(combined$t_4A4a1, combined$share_Poland)
temp_sd <- wtd.var(combined$t_4A4a1, combined$share_Poland) %>% sqrt()
combined$std_Poland_t_4A4a1 <- (combined$t_4A4a1 - temp_mean) / temp_sd

temp_mean <- wtd.mean(combined$t_4A4a1, combined$share_Spain)
temp_sd <- wtd.var(combined$t_4A4a1, combined$share_Spain) %>% sqrt()
combined$std_Spain_t_4A4a1 <- (combined$t_4A4a1 - temp_mean) / temp_sd

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

combined$Belgium_NRCA <- combined$std_Belgium_t_4A2a4 + combined$std_Belgium_t_4A2b2 + combined$std_Belgium_t_4A4a1
combined$Poland_NRCA <- combined$std_Poland_t_4A2a4 + combined$std_Poland_t_4A2b2 + combined$std_Poland_t_4A4a1
combined$Spain_NRCA <- combined$std_Spain_t_4A2a4 + combined$std_Spain_t_4A2b2 + combined$std_Spain_t_4A4a1

# And we standardise NRCA in a similar way.
temp_mean <- wtd.mean(combined$Belgium_NRCA, combined$share_Belgium)
temp_sd <- wtd.var(combined$Belgium_NRCA, combined$share_Belgium) %>% sqrt()
combined$std_Belgium_NRCA <- (combined$Belgium_NRCA - temp_mean) / temp_sd

temp_mean <- wtd.mean(combined$Poland_NRCA, combined$share_Poland)
temp_sd <- wtd.var(combined$Poland_NRCA, combined$share_Poland) %>% sqrt()
combined$std_Poland_NRCA <- (combined$Poland_NRCA - temp_mean) / temp_sd

temp_mean <- wtd.mean(combined$Spain_NRCA, combined$share_Spain)
temp_sd <- wtd.var(combined$Spain_NRCA, combined$share_Spain) %>% sqrt()
combined$std_Spain_NRCA <- (combined$Spain_NRCA - temp_mean) / temp_sd

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
combined$multip_Spain_NRCA <- (combined$std_Spain_NRCA * combined$share_Spain)
combined$multip_Belgium_NRCA <- (combined$std_Belgium_NRCA * combined$share_Belgium)
combined$multip_Poland_NRCA <- (combined$std_Poland_NRCA * combined$share_Poland)

# Step 2: sum it up (it basically becomes another weighted mean)
agg_Spain <- aggregate(combined$multip_Spain_NRCA,
  by = list(combined$TIME),
  FUN = sum, na.rm = TRUE
)
agg_Belgium <- aggregate(combined$multip_Belgium_NRCA,
  by = list(combined$TIME),
  FUN = sum, na.rm = TRUE
)
agg_Poland <- aggregate(combined$multip_Poland_NRCA,
  by = list(combined$TIME),
  FUN = sum, na.rm = TRUE
)

# We can plot it now!
plot(agg_Poland$x, xaxt = "n")
axis(1, at = seq(1, 40, 3), labels = agg_Poland$Group.1[seq(1, 40, 3)])

plot(agg_Spain$x, xaxt = "n")
axis(1, at = seq(1, 40, 3), labels = agg_Spain$Group.1[seq(1, 40, 3)])

plot(agg_Belgium$x, xaxt = "n")
axis(1, at = seq(1, 40, 3), labels = agg_Belgium$Group.1[seq(1, 40, 3)])


# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment
