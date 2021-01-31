# Create descriptives by tests (not people like other file)

# Libraries
library(data.table)

### Create analytical dataset ####

#  load clean test data
load(normalizePath("other_data/clean_testing.Rdata"))

# Subset liverpool LA and positive lft tests
lft_tests=testing[lft==1 & lad11cd=="E08000012"]

# Calculate age band
lft_tests[, age_group:=cut(age, breaks = c(0,14,34,69,110), 
                           include.lowest=T)]
lft_tests[, age:=NULL]
lft_tests[age_group=="[0,14]", age:="0-14"]
lft_tests[age_group=="(14,34]", age:="15-34"]
lft_tests[age_group=="(34,69]", age:="35-69"]
lft_tests[age_group=="(69,110]", age:="70+"]
lft_tests[, age_group:=NULL]

# Load IMD data
load(normalizePath("other_data/lsoa_risk.RData")) # Load
ag_risk_lsoa <- ag_risk_lsoa[,c("lsoa11", "eng_quint_imd", "ladcd")]
lft_tests<-merge(lft_tests,ag_risk_lsoa[ladcd=="E08000012"], by="lsoa11", all.x=T) # Join onto main data

# Subset data into time periods for analysis
lft_tests_pilot <- lft_tests[lft_tests$t_date >= "2020-11-06 00:00:00" & lft_tests$t_date < "2020-12-03 00:00:00"] # Pilot period
lft_tests_xmas <- lft_tests[lft_tests$t_date >= "2020-12-03 00:00:00" & lft_tests$t_date < "2021-01-06 00:00:00"] # Christmas period
lft_tests_lockdown <- lft_tests[lft_tests$t_date >= "2021-01-06 00:00:00"] # Lockdown

### Whole period ###

# Create summary table
table1 <- data.frame(Var1 = character(), Freq = numeric(), stringsAsFactors = FALSE) # Create blank table for storing results
table1[1,] <- c("Whole period", 0) # Data type
table1[2,] <- c("Total", nrow(lft_tests)) # Total

hold <- as.data.frame(table(lft_tests$sex)) # Sex
table1 <- rbind(table1, hold) # Join onto main table

hold <- as.data.frame(table(lft_tests$age)) # Repeat for age
table1 <- rbind(table1, hold)

hold <- as.data.frame(table(lft_tests$eth_group_imp)) # Repeat for ethnicity
table1 <- rbind(table1, hold)

# Repeat for IMD quintile
hold <- as.data.frame(table(lft_tests$eng_quint_imd)) # Repeat for ethnicity
table1 <- rbind(table1, hold)



### Pilot ###

# Create summary table
table2 <- data.frame(Var1 = character(), Freq = numeric(), stringsAsFactors = FALSE) # Create blank table for storing results
table2[1,] <- c("Pilot", 0) # Data type
table2[2,] <- c("Total", nrow(lft_tests_pilot)) # Total

hold <- as.data.frame(table(lft_tests_pilot$sex)) # Sex
table2 <- rbind(table2, hold) # Join onto main table

hold <- as.data.frame(table(lft_tests_pilot$age)) # Repeat for age
table2 <- rbind(table2, hold)

hold <- as.data.frame(table(lft_tests_pilot$eth_group_imp)) # Repeat for ethnicity
table2 <- rbind(table2, hold)

# Repeat for IMD quintile
hold <- as.data.frame(table(lft_tests_pilot$eng_quint_imd)) # Repeat for ethnicity
table1 <- rbind(table1, hold)


### Christmas ###

# Create summary table
table3 <- data.frame(Var1 = character(), Freq = numeric(), stringsAsFactors = FALSE) # Create blank table for storing results
table3[1,] <- c("Christmas", 0) # Data type
table3[2,] <- c("Total", nrow(lft_tests_xmas)) # Total

hold <- as.data.frame(table(lft_tests_xmas$sex)) # Sex
table3 <- rbind(table3, hold) # Join onto main table

hold <- as.data.frame(table(lft_tests_xmas$age)) # Repeat for age
table3 <- rbind(table3, hold)

hold <- as.data.frame(table(lft_tests_xmas$eth_group_imp)) # Repeat for ethnicity
table3 <- rbind(table3, hold)

# Repeat for IMD quintile
hold <- as.data.frame(table(lft_tests_xmas$eng_quint_imd)) # Repeat for ethnicity
table1 <- rbind(table1, hold)



### Lockdown ###

# Create summary table
table4 <- data.frame(Var1 = character(), Freq = numeric(), stringsAsFactors = FALSE) # Create blank table for storing results
table4[1,] <- c("Lockdown", 0) # Data type
table4[2,] <- c("Total", nrow(lft_tests_lockdown)) # Total

hold <- as.data.frame(table(lft_tests_lockdown$sex)) # Sex
table4 <- rbind(table4, hold) # Join onto main table

hold <- as.data.frame(table(lft_tests_lockdown$age)) # Repeat for age
table4 <- rbind(table4, hold)

hold <- as.data.frame(table(lft_tests_lockdown$eth_group_imp)) # Repeat for ethnicity
table4 <- rbind(table4, hold)

# Repeat for IMD quintile
hold <- as.data.frame(table(lft_tests_lockdown$eng_quint_imd)) # Repeat for ethnicity
table1 <- rbind(table1, hold)


### Combine ###

table1 <- rbind(table1, table2, table3, table4)
rm(table2, table3, table4, hold)

write.csv(table1, "./output/descriptives_by_tests.csv")
