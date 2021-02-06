###########################
### Multi-level models  ###
###########################

# Purpose: To analyse use a multi-level modelling framework to undertake individual-level analysis as sensitivity analyses.

# Libraries
library(data.table)
library(lme4)

### 1. Create analytical dataset ####

#  load clean test data
load(normalizePath("other_data/clean_testing.Rdata"))

# Drop all tests where age <= 5 (there should not be any, so assume erroneous data)
testing <- testing[age>5]

# Subset liverpool LA and lft tests
lft_tests=testing[lft==1 & lad11cd=="E08000012" & t_date >= "2020-11-06 00:00:00" & t_date < "2021-02-01 00:00:00"]
rm(testing)


### 2. Join on spatial data ###

# Load covariates for analysis
# Ben's data
load(normalizePath("other_data/lsoa_risk.RData")) # Load
ag_risk_lsoa$prop_students <- ag_risk_lsoa$prop_students / 100 # Variable is percentage, so convert to proportion for consistency
ag_risk_lsoa$ch_prop <- ag_risk_lsoa$ch_prop / 100 # Same for care home beds
# Recode ch_prop as binary (care home in LSOA)
ag_risk_lsoa$ch_binary <- NA
ag_risk_lsoa$ch_binary[ag_risk_lsoa$ch_prop == 0] <- 0
ag_risk_lsoa$ch_binary[ag_risk_lsoa$ch_prop > 0] <- 1
ag_risk_lsoa$grp_label <- as.factor(ag_risk_lsoa$grp_label) # To define as factor
ag_risk_lsoa$grp_label <- relevel(ag_risk_lsoa$grp_label, ref = "e-Veterans") # Set reference group
# Accessibility data
access <- read.csv("./other_data/distance_to_nearest_site_18Jan21.csv") # Access to test sites post pilot
access$distance <- access$distance / 1000 # Convert to km
ag_risk_lsoa <- merge(ag_risk_lsoa, access, by = "lsoa11", all.x = TRUE) # Join onto main file
access2 <- read.csv("./other_data/uptake data/access_wtdavg_lsoa.csv") # Access to test site during pilot
access2 <- access2[,c("lsoa11cd", "walkDistAvg")] # Drop variables dont need
access2$walkDistAvg <- access2$walkDistAvg / 1000 # Convert to km
ag_risk_lsoa <- merge(ag_risk_lsoa, access2, by.x = "lsoa11", by.y = "lsoa11cd", all.x = TRUE) # Join onto main file
rm(access, access2)

# Join onto testing data
lft_tests <- merge(lft_tests,ag_risk_lsoa[ladcd=="E08000012"], by="lsoa11", all.x=T)

# Define how many tests
lft_tests[order(t_date), num_test:=1:.N, by=.(pid)] # Calculate number of tests

# Aggregate to people taking only variables we need for models (max for all as wont change over number of tests anyway)
lft_persons <- lft_tests[, list(positive = max(positive), age = max(age), imd_score = max(imd_score), prop_students = max(prop_students), ch_binary = max(ch_binary), walkDistAvg = max(walkDistAvg), num_test = max(num_test), t_date = max(t_date)), by = list(pid, sex, lsoa11, grp_label, eth_group_imp)]
rm(lft_tests, ag_risk_lsoa)

# Some people above have different addresses giving different values, so select first so 1 person only
lft_persons[order(t_date), number:=1:.N, by=.(pid)] # Calculate number of tests
lft_persons<-lft_persons[number==1] # Select first

# Define multiple tests or not
lft_persons$multiple_lfts <- NA
lft_persons$multiple_lfts[lft_persons$num_test == 1] <- 0
lft_persons$multiple_lfts[lft_persons$num_test > 1] <- 1

# Select reference groups
lft_persons$eth_group_imp <- as.factor(lft_persons$eth_group_imp)
lft_persons$eth_group_imp <- relevel(lft_persons$eth_group_imp, ref = "white") # Set refs
lft_persons$grp_label <- relevel(lft_persons$grp_label, ref = "e-Veterans") 

# Standardise/centre continuous variables
lft_persons$zimd_score <- scale(lft_persons$imd_score, center = TRUE, scale = TRUE)
lft_persons$zwalkDistAvg <- scale(lft_persons$walkDistAvg, center = TRUE, scale = TRUE)
lft_persons$zprop_students <- scale(lft_persons$prop_students, center = TRUE, scale = TRUE)

### 3. Multi-level model - positivity ###

# Model
#model1 <- glmer(positive ~ 1 + age + sex + factor(eth_group_imp) + imd_score + prop_students + ch_binary + walkDistAvg + factor(grp_label) + (1|lsoa11), family="binomial", data = lft_persons)
model1 <- glmer(positive ~ 1 + age + sex + factor(eth_group_imp) + zimd_score + zprop_students + ch_binary + (1|lsoa11), family="binomial", data = lft_persons)
summary(model1)


### 4. Multi-level model - multiple LFTs ###

# Model
model2 <- glmer(multiple_lfts ~ 1 + age + sex + factor(eth_group_imp) + zimd_score + zprop_students + ch_binary +  zwalkDistAvg + factor(grp_label) + (1|lsoa11), family="binomial", data = lft_persons)
summary(model2)
