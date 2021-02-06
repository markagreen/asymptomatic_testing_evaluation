# Code to clean data into analytical format

# This code is a bit of a mess and I apologise for it entirely (but it does work!)

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

# Could be made more efficient with a loop, sorry not sorry

### Clean overall dataset ###

#  just select first test
#lft_tests[order(t_date), num_test:=1:.N, by=.(pid)]
#lft_tests<-lft_tests[num_test==1]

# change imputed ethnicity to charatcetr
lft_tests[, eth_group_imp:=as.character(eth_group_imp)]

# age group - can change these at the moment quite broad - 


lft_tests[, age_group:=cut(age, breaks = c(0,14,34,69,110), 
                           include.lowest=T)]
lft_tests[, age:=NULL]
lft_tests[age_group=="[0,14]", age:="0-14"]
lft_tests[age_group=="(14,34]", age:="15-34"]
lft_tests[age_group=="(34,69]", age:="35-69"]
lft_tests[age_group=="(69,110]", age:="70+"]
lft_tests[, age_group:=NULL]
with(lft_tests,table(age, useNA = "ifany"))

# covert sex to numeric
lft_tests[sex=="Male", sexn:=1]
lft_tests[sex=="Female", sexn:=2]

# agregate
lsoa_lft<-lft_tests[,list(lft=sum(lft, na.rm =T )), by=.(age,sexn,eth_group_imp,lsoa11,lad11cd)]

# Load number of tests
lft_no_tests=testing[lft==1 & lad11cd=="E08000012"] # Subset only liverpool lfts

# Clean in same way as above
lft_no_tests[, eth_group_imp:=as.character(eth_group_imp)] # Ethnicity

lft_no_tests[, age_group:=cut(age, breaks = c(0,14,34,69,110), # Age
                           include.lowest=T)]
lft_no_tests[, age:=NULL]
lft_no_tests[age_group=="[0,14]", age:="0-14"]
lft_no_tests[age_group=="(14,34]", age:="15-34"]
lft_no_tests[age_group=="(34,69]", age:="35-69"]
lft_no_tests[age_group=="(69,110]", age:="70+"]
lft_no_tests[, age_group:=NULL]

lft_no_tests[sex=="Male", sexn:=1] # Sex
lft_no_tests[sex=="Female", sexn:=2]

# Aggregate
tests<-lft_no_tests[,list(lft=sum(lft, na.rm =T )), by=.(age,sexn,eth_group_imp,lsoa11,lad11cd)]

# Merge to number of tests
lsoa_lft<-merge(lsoa_lft,tests, by=c("age","sexn","eth_group_imp","lsoa11"), all=T)

#  if no lfts from cell - this should be zero
lsoa_lft[is.na(lft.x)==T, lft.x:=0]


#  deriving expected number based on age , sex and ethnicity profile
#  age/sex/ethnicity specific rates
liv_lft<-lsoa_lft[,list(lft=sum(lft.x, na.rm =T ), pop=sum(lft.y, na.rm = T)), 
                  by=.(age,sexn,eth_group_imp,lad11cd=lad11cd.y)]



liv_lft[, rate:=lft/pop]

exp_liv<-merge(tests,liv_lft[, .(age,sexn,eth_group_imp,rate)])
#  expected number in each lsoa - based on age/sex/ethnicity and applied to number of LFT tests
exp_liv[, exp_num:=rate*lft]

exp_liv<-exp_liv[, list(exp_num=round(sum(exp_num))), by=.(lsoa11)]


lsoa_lft_all<-lft_tests[,list(lft=sum(lft, na.rm =T )), by=.(lsoa11,lad11cd)]

#  merge in the expected number 
lsoa_lft_all<-merge(lsoa_lft_all,exp_liv, by="lsoa11", all.y = T)
lsoa_lft_all<-merge(lsoa_lft_all,ag_risk_lsoa[ladcd=="E08000012"], by="lsoa11", all.x=T)

# Add in 0s for NAs (due to calculation process this is correct)
lsoa_lft_all$lft[is.na(lsoa_lft_all$lft)] <- 0

### Clean pilot period ###


#  just select first test
#lft_tests_pilot[order(t_date), num_test:=1:.N, by=.(pid)]
#lft_tests_pilot<-lft_tests_pilot[num_test==1]

# change imputed ethnicity to charatcetr
lft_tests_pilot[, eth_group_imp:=as.character(eth_group_imp)]

# age group - can change these at the moment quite broad - 


lft_tests_pilot[, age_group:=cut(age, breaks = c(0,14,34,69,110), 
                           include.lowest=T)]
lft_tests_pilot[, age:=NULL]
lft_tests_pilot[age_group=="[0,14]", age:="0-14"]
lft_tests_pilot[age_group=="(14,34]", age:="15-34"]
lft_tests_pilot[age_group=="(34,69]", age:="35-69"]
lft_tests_pilot[age_group=="(69,110]", age:="70+"]
lft_tests_pilot[, age_group:=NULL]
with(lft_tests_pilot,table(age, useNA = "ifany"))

# covert sex to numeric
lft_tests_pilot[sex=="Male", sexn:=1]
lft_tests_pilot[sex=="Female", sexn:=2]

# agregate
lsoa_lft<-lft_tests_pilot[,list(lft=sum(lft, na.rm =T )), by=.(age,sexn,eth_group_imp,lsoa11,lad11cd)]

# Load number of tests
lft_no_tests=testing[lft==1 & lad11cd=="E08000012" & t_date >= "2020-11-06 00:00:00" & t_date < "2020-12-03 00:00:00"] # Subset only liverpool lfts in the date

# Clean in same way as above
lft_no_tests[, eth_group_imp:=as.character(eth_group_imp)] # Ethnicity

lft_no_tests[, age_group:=cut(age, breaks = c(0,14,34,69,110), # Age
                              include.lowest=T)]
lft_no_tests[, age:=NULL]
lft_no_tests[age_group=="[0,14]", age:="0-14"]
lft_no_tests[age_group=="(14,34]", age:="15-34"]
lft_no_tests[age_group=="(34,69]", age:="35-69"]
lft_no_tests[age_group=="(69,110]", age:="70+"]
lft_no_tests[, age_group:=NULL]

lft_no_tests[sex=="Male", sexn:=1] # Sex
lft_no_tests[sex=="Female", sexn:=2]

# Aggregate
tests<-lft_no_tests[,list(lft=sum(lft, na.rm =T )), by=.(age,sexn,eth_group_imp,lsoa11,lad11cd)]

# Merge to number of tests
lsoa_lft<-merge(lsoa_lft,tests, by=c("age","sexn","eth_group_imp","lsoa11"), all=T)

#  if no lfts from cell - this should be zero
lsoa_lft[is.na(lft.x)==T, lft.x:=0]


#  deriving expected number based on age , sex and ethnicity profile
#  age/sex/ethnicity specific rates
liv_lft<-lsoa_lft[,list(lft=sum(lft.x, na.rm =T ), pop=sum(lft.y, na.rm = T)), 
                  by=.(age,sexn,eth_group_imp,lad11cd=lad11cd.y)]



liv_lft[, rate:=lft/pop]

exp_liv<-merge(tests,liv_lft[, .(age,sexn,eth_group_imp,rate)])
#  expected number in each lsoa - based on age/sex/ethnicity and applied to number of LFT tests
exp_liv[, exp_num:=rate*lft]

exp_liv<-exp_liv[, list(exp_num=round(sum(exp_num))), by=.(lsoa11)]


lsoa_lft_pilot<-lft_tests_pilot[,list(lft=sum(lft, na.rm =T )), by=.(lsoa11,lad11cd)]

#  merge in the expected number 
lsoa_lft_pilot<-merge(lsoa_lft_pilot,exp_liv, by="lsoa11", all.y = T)
lsoa_lft_pilot<-merge(lsoa_lft_pilot,ag_risk_lsoa[ladcd=="E08000012"], by="lsoa11", all.x=T)

# Add in 0s for NAs (due to calculation process this is correct)
lsoa_lft_pilot$lft[is.na(lsoa_lft_pilot$lft)] <- 0


### Clean xmas period ###


#  just select first test
#lft_tests_xmas[order(t_date), num_test:=1:.N, by=.(pid)]
#lft_tests_xmas<-lft_tests_xmas[num_test==1]

# change imputed ethnicity to charatcetr
lft_tests_xmas[, eth_group_imp:=as.character(eth_group_imp)]

# age group - can change these at the moment quite broad - 


lft_tests_xmas[, age_group:=cut(age, breaks = c(0,14,34,69,110), 
                                include.lowest=T)]
lft_tests_xmas[, age:=NULL]
lft_tests_xmas[age_group=="[0,14]", age:="0-14"]
lft_tests_xmas[age_group=="(14,34]", age:="15-34"]
lft_tests_xmas[age_group=="(34,69]", age:="35-69"]
lft_tests_xmas[age_group=="(69,110]", age:="70+"]
lft_tests_xmas[, age_group:=NULL]
with(lft_tests_xmas,table(age, useNA = "ifany"))

# covert sex to numeric
lft_tests_xmas[sex=="Male", sexn:=1]
lft_tests_xmas[sex=="Female", sexn:=2]

# agregate
lsoa_lft<-lft_tests_xmas[,list(lft=sum(lft, na.rm =T )), by=.(age,sexn,eth_group_imp,lsoa11,lad11cd)]

# Load number of tests
lft_no_tests=testing[lft==1 & lad11cd=="E08000012" & t_date >= "2020-12-03 00:00:00" & t_date < "2021-01-06 00:00:00"] # Subset only liverpool lfts in the date

# Clean in same way as above
lft_no_tests[, eth_group_imp:=as.character(eth_group_imp)] # Ethnicity

lft_no_tests[, age_group:=cut(age, breaks = c(0,14,34,69,110), # Age
                              include.lowest=T)]
lft_no_tests[, age:=NULL]
lft_no_tests[age_group=="[0,14]", age:="0-14"]
lft_no_tests[age_group=="(14,34]", age:="15-34"]
lft_no_tests[age_group=="(34,69]", age:="35-69"]
lft_no_tests[age_group=="(69,110]", age:="70+"]
lft_no_tests[, age_group:=NULL]

lft_no_tests[sex=="Male", sexn:=1] # Sex
lft_no_tests[sex=="Female", sexn:=2]

# Aggregate
tests<-lft_no_tests[,list(lft=sum(lft, na.rm =T )), by=.(age,sexn,eth_group_imp,lsoa11,lad11cd)]

# Merge to number of tests
lsoa_lft<-merge(lsoa_lft,tests, by=c("age","sexn","eth_group_imp","lsoa11"), all=T)

#  if no lfts from cell - this should be zero
lsoa_lft[is.na(lft.x)==T, lft.x:=0]


#  deriving expected number based on age , sex and ethnicity profile
#  age/sex/ethnicity specific rates
liv_lft<-lsoa_lft[,list(lft=sum(lft.x, na.rm =T ), pop=sum(lft.y, na.rm = T)), 
                  by=.(age,sexn,eth_group_imp,lad11cd=lad11cd.y)]



liv_lft[, rate:=lft/pop]

exp_liv<-merge(tests,liv_lft[, .(age,sexn,eth_group_imp,rate)])
#  expected number in each lsoa - based on age/sex/ethnicity and applied to number of LFT tests
exp_liv[, exp_num:=rate*lft]

exp_liv<-exp_liv[, list(exp_num=round(sum(exp_num))), by=.(lsoa11)]


lsoa_lft_xmas<-lft_tests_xmas[,list(lft=sum(lft, na.rm =T )), by=.(lsoa11,lad11cd)]

#  merge in the expected number 
lsoa_lft_xmas<-merge(lsoa_lft_xmas,exp_liv, by="lsoa11", all.y = T)
lsoa_lft_xmas<-merge(lsoa_lft_xmas,ag_risk_lsoa[ladcd=="E08000012"], by="lsoa11", all.x=T)

# Add in 0s for NAs (due to calculation process this is correct)
lsoa_lft_xmas$lft[is.na(lsoa_lft_xmas$lft)] <- 0


### Clean lockdown period ###


#  just select first test
#lft_tests_lockdown[order(t_date), num_test:=1:.N, by=.(pid)]
#lft_tests_lockdown<-lft_tests_lockdown[num_test==1]

# change imputed ethnicity to charatcetr
lft_tests_lockdown[, eth_group_imp:=as.character(eth_group_imp)]

# age group - can change these at the moment quite broad - 


lft_tests_lockdown[, age_group:=cut(age, breaks = c(0,14,34,69,110), 
                               include.lowest=T)]
lft_tests_lockdown[, age:=NULL]
lft_tests_lockdown[age_group=="[0,14]", age:="0-14"]
lft_tests_lockdown[age_group=="(14,34]", age:="15-34"]
lft_tests_lockdown[age_group=="(34,69]", age:="35-69"]
lft_tests_lockdown[age_group=="(69,110]", age:="70+"]
lft_tests_lockdown[, age_group:=NULL]
with(lft_tests_lockdown,table(age, useNA = "ifany"))

# covert sex to numeric
lft_tests_lockdown[sex=="Male", sexn:=1]
lft_tests_lockdown[sex=="Female", sexn:=2]

# agregate
lsoa_lft<-lft_tests_lockdown[,list(lft=sum(lft, na.rm =T )), by=.(age,sexn,eth_group_imp,lsoa11,lad11cd)]

# Load number of tests
lft_no_tests=testing[lft==1 & lad11cd=="E08000012" & t_date >= "2021-01-06 00:00:00"] # Subset only liverpool lfts in the date

# Clean in same way as above
lft_no_tests[, eth_group_imp:=as.character(eth_group_imp)] # Ethnicity

lft_no_tests[, age_group:=cut(age, breaks = c(0,14,34,69,110), # Age
                              include.lowest=T)]
lft_no_tests[, age:=NULL]
lft_no_tests[age_group=="[0,14]", age:="0-14"]
lft_no_tests[age_group=="(14,34]", age:="15-34"]
lft_no_tests[age_group=="(34,69]", age:="35-69"]
lft_no_tests[age_group=="(69,110]", age:="70+"]
lft_no_tests[, age_group:=NULL]

lft_no_tests[sex=="Male", sexn:=1] # Sex
lft_no_tests[sex=="Female", sexn:=2]

# Aggregate
tests<-lft_no_tests[,list(lft=sum(lft, na.rm =T )), by=.(age,sexn,eth_group_imp,lsoa11,lad11cd)]

# Merge to number of tests
lsoa_lft<-merge(lsoa_lft,tests, by=c("age","sexn","eth_group_imp","lsoa11"), all=T)

#  if no lfts from cell - this should be zero
lsoa_lft[is.na(lft.x)==T, lft.x:=0]


#  deriving expected number based on age , sex and ethnicity profile
#  age/sex/ethnicity specific rates
liv_lft<-lsoa_lft[,list(lft=sum(lft.x, na.rm =T ), pop=sum(lft.y, na.rm = T)), 
                  by=.(age,sexn,eth_group_imp,lad11cd=lad11cd.y)]



liv_lft[, rate:=lft/pop]

exp_liv<-merge(tests,liv_lft[, .(age,sexn,eth_group_imp,rate)])
#  expected number in each lsoa - based on age/sex/ethnicity and applied to number of LFT tests
exp_liv[, exp_num:=rate*lft]

exp_liv<-exp_liv[, list(exp_num=round(sum(exp_num))), by=.(lsoa11)]


lsoa_lft_lockdown<-lft_tests_lockdown[,list(lft=sum(lft, na.rm =T )), by=.(lsoa11,lad11cd)]

#  merge in the expected number 
lsoa_lft_lockdown<-merge(lsoa_lft_lockdown,exp_liv, by="lsoa11", all.y = T)
lsoa_lft_lockdown<-merge(lsoa_lft_lockdown,ag_risk_lsoa[ladcd=="E08000012"], by="lsoa11", all.x=T)

# Add in 0s for NAs (due to calculation process this is correct)
lsoa_lft_lockdown$lft[is.na(lsoa_lft_lockdown$lft)] <- 0

# Tidy up at end
rm(exp_liv, ag_risk_lsoa, liv_lft, lsoa_lft, tests, testing)
