##############################
### Create plots for paper ###
##############################

# Purpose: To produce a series of summary plots that are included in the main paper, supplementary appendices, or were generated during the analysis process to help investigate the data.

# Libraries
library(data.table) # For data wrangling (plus those below)
library(lubridate) 
library(tidyr)
library(plyr)
library(ggplot2) # For visualisations
library(patchwork)
library(zoo) # For rolling average

### 1. Tidy data ####

#  Load cleaned test data
load(normalizePath("../other_data/clean_testing.Rdata"))

# Subset dates
testing$t_date <- ymd_hms(testing$t_date) # Convert to time-date
testing <- testing[testing$t_date >= "2020-11-06 00:00:00",]
testing$day <- as_date(testing$t_date) # Define day

# Subset Liverpool Local Authority data
testing <- testing[testing$lad11cd == "E08000012",] 

# Create single test type variable
testing$test <- NA
testing$test[testing$lft == 1] <- "lft"
testing$test[testing$pcr == 1] <- "pcr"

### 2. Create trends in uptake/positivity plot (Figure 1) ###

# Aggregate to number of tests per day
tests_day <- testing[, list(positive = sum(positive, na.rm = T), negative = sum(negative, na.rm = T), total = .N), by = c("day", "test")] # Aggregate to get total numbers for positive, negative and overall by test type

# Percentage of tests per day that were positive
tests_day$positivity <- (tests_day$positive / (tests_day$negative + tests_day$positive)) * 100 # Not using the total here since that includes some NAs for results in 'testing' - these are tests that were void or insufficient, so just calculated percentage by known positive/negative results 

# Create 7 day rolling averages for plotting purposes
tests_day <- tests_day %>%
  dplyr::arrange(day) %>% # Order by day (ascending order)
  dplyr::group_by(test) %>% # Group by test
  dplyr::mutate(total_7day = zoo::rollmean(total, k = 7, fill = NA), # Create rolling average for total tests
                positivity_7day = zoo::rollmean(positivity, k = 7, fill = NA)) # Repeat for positivity

# Figure 1

# Add labels for plot 
text_plot <- data.frame(text = c("Pilot begins", "Pilot ends", "Christmas", "Lockdown"), dates = as.Date(c("2020-11-06", "2020-12-03", "2020-12-25", "2021-01-05")), stringsAsFactors = FALSE)

# Plot 1a
p1a <- ggplot(tests_day[tests_day$test == "lft",], aes(x = day, y = total)) +
  geom_point() +
  geom_line(aes(y=total_7day)) + # 7 day rolling average
  # geom_smooth() + # If want to have smoothed average instead of above
  geom_vline(mapping = aes(xintercept = dates), data = text_plot, show.legend = FALSE, linetype = "dotted") + # Add in dotted lines for periods (or try "dotted")
  geom_text(mapping = aes(x = dates, y = 12500, label = text, hjust = -0.05, vjust = -0.5), data = text_plot) + # Add in text labels for period
  xlab("Date") +
  ylab("Total number of tests") +
  ylim(0,14000)

# Plot 1b
p1b <- ggplot(tests_day[tests_day$test == "lft",], aes(x = day, y = positivity)) +
  geom_point() + # Plot daily value
  geom_line(aes(y=positivity_7day)) + # 7 day rolling average
  # geom_smooth() + # If want to have smoothed average instead of above
  geom_vline(mapping = aes(xintercept = dates), data = text_plot, show.legend = FALSE, linetype = "dotted") + # Add in dotted lines for periods (or try "dotted")
  geom_text(mapping = aes(x = dates, y = 6, label = text, hjust = -0.05, vjust = -0.5), data = text_plot) + # Add in text labels for period
  xlab("Date") +
  ylab("Percentage of tests") +
  ylim(0,6.5)

# Join plots together
fig1 <- p1a / p1b + # Have two plots, one on top of the other
  plot_annotation(tag_levels = "A") # Add on A and B labels to plots
fig1 # Print

# Save
ggsave(plot = fig1, filename = "../output/trends_lft_highres.tiff", dpi = 300)
ggsave(plot = fig1, filename = "../output/trends_lft_lowres.jpeg")

# # If want to create same plot but for LFTs and PCRs together (not in paper)
# 
# # Plot a
# p1a_2 <- ggplot(tests_day_wide, aes(x = day, y = total, group = test, color = test)) +
#   geom_point() +
#   geom_smooth() +
#   xlab("Date") +
#   ylab("Total number of tests")  +
#   labs(color = "Test")
# 
# # Plot b
# p1b_2 <- ggplot(tests_day_wide, aes(x = day, y = positivity, group = TestKit, color = TestKit)) +
#   geom_point() +
#   geom_smooth() +
#   xlab("Date") +
#   ylab("Percentage of tests that were positive") +
#   labs(color = "Test")
# 
# # Join together
# fig1_2 <- p1a_2 + p1b_2 + plot_layout(guides = "collect")
# fig1_2
# 
# # Save
# ggsave(plot = fig1_2, filename = "./Plots/trends_plot_highres.tiff", dpi = 300)
# ggsave(plot = fig1_2, filename = "./Plots/trends_plot_lowres.jpeg")


### 3. 10 year age band plot (Appendix B) ###

# Aggregate dataset to number of persons rather than tests
dt <- data.table(testing) # Convert format of data
persons_data <- dt[, list(number_tests = .N), by = c("pid", "test")] # Aggregate
rm(dt)

# Join back on key personal data about individuals
chars <- testing[,c("pid", "age", "sex", "eth_group_imp", "lsoa11")] # Select key variables to store
chars <- chars[!duplicated(chars$pid), ] # Remove duplicates
persons_data <- merge(persons_data, chars, by = "pid", all.x = TRUE) # Join onto main data
rm(chars) # Save space

# Create age group variable
persons_data$age_band <- cut(persons_data$age, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 200), right = FALSE)

# Aggregate data
agg_age <- persons_data[, list(Frequency = .N), by = c("test", "age_band")] # Aggregate
table_age <- spread(agg_age, test, Frequency) # Reshape data to wide format
rm(agg_age, persons_data)

# Recode the age bands for presentation purposes
table_age$age_band <- revalue(table_age$age_band, c("[0,10)"="0-9", "[10,20)"="10-19", "[20,30)"="20-29", "[30,40)"="30-39", "[40,50)"="40-49", "[50,60)"="50-59", "[60,70)"="60-69", "[70,80)"="70-79", "[80,200)"="80+"))

# Add in population counts (via 2019 mid year ONS population estimates)
table_age$Population <- NA
table_age$Population[table_age$age_band == "0-9"] <- 57608
table_age$Population[table_age$age_band == "10-19"] <- 54662
table_age$Population[table_age$age_band == "20-29"] <- 99315
table_age$Population[table_age$age_band == "30-39"] <- 73312
table_age$Population[table_age$age_band == "40-49"] <- 54758
table_age$Population[table_age$age_band == "50-59"] <- 59255
table_age$Population[table_age$age_band == "60-69"] <- 47390
table_age$Population[table_age$age_band == "70-79"] <- 31830
table_age$Population[table_age$age_band == "80+"] <- 19912

# Add in percentage variables
table_age$lft_percentage <- (table_age$lft / table_age$Population) * 100
table_age$pcr_percentage <- (table_age$pcr / table_age$Population) * 100

# Plot
plot_age <- ggplot(table_age, aes(x = age_band, y = lft_percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = lft), hjust = 0.5, vjust = -1) +
  xlab("Age group") +
  ylab("Percentage of population who received a test") +
  ylim(0,60)
plot_age # Print
ggsave(file="./output/age_plot.jpeg", plot=plot_age) # Save



### 4. Symptomatic tests (Appendix B) ###

# Calculate number of symptomatic LFTs 
symp_tests_day <- testing[, list(total = .N), by = c("day", "test", "symptoms")] # Aggregate to get total numbers for positive, negative and overall by test type
symp_tests_day_wide <- pivot_wider(symp_tests_day, names_from = symptoms, values_from = total) # Reshape
symp_tests_day_wide$`TRUE`[is.na(symp_tests_day_wide$`TRUE`)] <- 0# If missing data, then should be 0
symp_tests_day_wide$total <- symp_tests_day_wide$`FALSE` + symp_tests_day_wide$`TRUE`
symp_tests_day_wide$percent_symp <- (symp_tests_day_wide$`TRUE` / symp_tests_day_wide$total) * 100
rm(symp_tests_day)

# Create 7 day rolling averages for plotting purposes
symp_tests_day_wide <- symp_tests_day_wide %>%
  dplyr::arrange(day) %>% # Order by day (ascending order)
  dplyr::group_by(test) %>% # Group by test
  dplyr::mutate(positive_7day = zoo::rollmean(percent_symp, k = 7, fill = NA)) # Create rolling average for total tests

# # Plot for LFT and PCR together
# plot_symp <- ggplot(symp_tests_day_wide, aes(x = day, y = percent_symp, group=TestKit, color=TestKit)) +
#   geom_point() +
#   geom_smooth() +
#   xlab("Date") +
#   ylab("Percentage of tests where person had COVID-19 symptoms") +
#   ylim(0,60)
# 
# ggsave(plot = plot_symp, filename = "./output/trends_symptoms_highres.tiff", dpi = 300)
# ggsave(plot = plot_symp, filename = "./output/trends_symptoms_lowres.jpeg")

# Plot for just LFTs
plot_symp2 <- ggplot(symp_tests_day_wide[symp_tests_day_wide$test == "lft",], aes(x = day, y = percent_symp)) +
  geom_point() +
  geom_line(aes(y=positive_7day)) + # 7 day rolling average
  geom_vline(mapping = aes(xintercept = dates), data = text_plot, show.legend = FALSE, linetype = "dotted") + # Add in dotted lines for periods (or try "dotted")
  geom_text(mapping = aes(x = dates, y = 1.2, label = text, hjust = -0.05, vjust = -0.5), data = text_plot) + # Add in text labels for period
  xlab("Date") +
  ylab("Percentage of tests") +
  ylim(0,1.2)
plot_symp2 # Print

ggsave(plot = plot_symp2, filename = "../output/trends_lft_symptoms_highres.tiff", dpi = 300)
ggsave(plot = plot_symp2, filename = "../output/trends_lft_symptoms_lowres.jpeg")

# Summary statistics/
sum(symp_tests_day_wide$`TRUE`[symp_tests_day_wide$test == "lft"], na.rm=T)
sum(symp_tests_day_wide$total[symp_tests_day_wide$test == "lft"], na.rm=T)
