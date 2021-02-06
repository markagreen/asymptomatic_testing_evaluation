
# Libraries
# install.packages("bit64") # Need but not sure why
library(data.table)
library(lubridate)
library(missMDA)
library(mice)
library(bit64)

# clean test data

# Load data
if (!file.exists(normalizePath("other_data/testing.Rdata"))) {
testing <- fread("../01-02-21/COVIDPILLAR2_VCam_MastP2.txt") # All testing data
save(testing,file=normalizePath("other_data/testing.Rdata"))
}

# Might need this to make fread faster
# testing <- fread("/Volumes/MAST/13-01-21/COVIDPILLAR2_VCam_MastP2.txt", # Select only these columns
#                  select = c("PseudoID", "TestRecordCreatedDate", "TestKit", "TestResultDesc", "Age", "Gender", "Ethnicity", "LLSOA2011", "LowerTierLocalAuthority", "CovidSymptomatic")) # nrows=1 only loads in first line

load(normalizePath("./other_data/testing.Rdata"))
#  format date as date format
testing[, t_date:=ymd_hms(TestRecordCreatedDate)]


#  create dummy for lft
testing[, lft:=as.numeric(tolower(TestKit)=="lft")]


#  create dummy fr pcr assuming all that are not lft are pct
testing[, pcr:=as.numeric(lft==0)]


#  Age goes up to 149??  

#  replae all ages over 100 with 100 
testing[ Age>100, Age:=100]

#  Ethnicity  - some of the categories look a bit non-standard
with(testing, table(Ethnicity))

#  convert all to lower case

testing[, Ethnicity:=tolower(Ethnicity)] 

#  create broad ethnic group category 

white <- c("any other white background","another white background","white other", "british", 
           "british~ english~ northern irish~ scottish~ or welsh", "white", "white british","irish","Irish Traveller or Gypsy") 

black<-c("african", "another black background", "any other black / african / caribbean background" , "black - african", "caribbean", "black~ african~ black british or caribbean")

asian<-c("asian or asian british","another asian background", "any other asian background", "bangladeshi", "indian" , "pakistani", "chinese","other asian / asian british")


mixed<-c(" black caribbean and white", "black african and white", "mixed or multiple ethnic groups", "any other mixed / multiple ethnic background","another mixed background", "white and black african"," white and asian","asian and white")

other<-c("another ethnic background", "another ethnic group", "any other ethnic category", "any other ethnic group", "arab")

testing[grepl(paste(white, collapse="|"), Ethnicity)==T, eth_group:="white" ]
testing[grepl(paste(black, collapse="|"), Ethnicity)==T, eth_group:="black" ]
testing[grepl(paste(asian, collapse="|"), Ethnicity)==T, eth_group:="asian" ]
testing[grepl(paste(mixed, collapse="|"), Ethnicity)==T, eth_group:="mixed" ]
testing[grepl(paste(other, collapse="|"), Ethnicity)==T, eth_group:="other" ]

# check any coded ethnicity that havent been categorised. 
table(testing[is.na(eth_group)==T]$Ethnicity, useNA = "ifany")

with(testing, prop.table(table(eth_group, useNA = "ifany")))
with(testing, table(eth_group, useNA = "ifany"))

#  14% missing ?? 
## Calculate statistics for number of tests ##


# options(scipen = 0)
# testing[, pid:=as.character(as.numeric(PseudoID))]
# testing$pid
# 
# 
# testing[, numtest_perpsid:=.N, by=.(pid)]
# with(testing, table(numtest_perpsid, useNA="ifany"
#                       ))
# 
# testing[numtest_perpsid==31306, .(PseudoID,Age,Gender,Ethnicity)]

# MG edit

#df <- data.frame(table(testing$FK_Subject_ID))
#table(df$Freq) # 1345545 have 1 test (98%), 11882 2 tests, 6 have 3, 9 have 4 and 1 has 5.
#df <- data.frame(table(testing$PseudoID))
#table(df$Freq) # 529695 have 1 test (39%), then goes up to 46! This feels more right?

# Issue is that PseudoID causes problems with the code below and limits imputation of ethnicity

# is ethnicity missing  for some records of the same person 
# take ethnicity from elsewhere in person record if available. 

testing[, eth_group2:=as.numeric(as.factor(eth_group))]

levels(as.factor(testing$eth_group))

testing[, eth_group2:=min(eth_group2, na.rm = T), by=.( PseudoID)]

testing[eth_group2==Inf, eth_group2:=NA]

testing[,eth_group:=as.character(factor(eth_group2, labels=levels(as.factor(testing$eth_group)))) ]

testing[, eth_group2:=NULL]
# doesnt make any difference - does if using PseudoID
with(testing, prop.table(table(eth_group, useNA = "ifany")))


testing[, numtest_perpsid:=.N, by=.(PseudoID)]
with(testing, table(numtest_perpsid, useNA="ifany"
 ))

#  Work status 86% missing
# with(testing, prop.table(table(WorkStatus,useNA = "ifany" )))

#  Work status 86% missing
# with(testing, prop.table(table(WorkOrStudyStatus,useNA = "ifany" )))


#  clean up testing result data


with(testing, table(TestResultDesc))

testing[tolower(TestResultDesc)=="lft insufficent",test_result:="insuff" ]
testing[tolower(TestResultDesc)=="lft negative",test_result:="negative" ]
testing[tolower(TestResultDesc)=="lft positive",test_result:="positive" ]
testing[tolower(TestResultDesc)=="void",test_result:="insuff" ]
testing[tolower(TestResultDesc)=="negative",test_result:="negative" ]
testing[tolower(TestResultDesc)=="positive",test_result:="positive" ]

with(testing, table(test_result, useNA = "ifany"))

testing[test_result!="insuff", positive:=as.numeric(test_result=="positive")]
testing[test_result!="insuff", negative:=as.numeric(test_result=="negative")]

with(testing[is.na(test_result)==T], table(TestResultDesc, useNA = "ifany"))

hold <- testing
testing<-testing[, .(pid= PseudoID,age=Age,t_date,lft,pcr,eth_group, sex=Gender,lsoa11=LLSOA2011, test_result,positive,negative, lad11cd=LowerTierLocalAuthority, symptoms = CovidSymptomatic)]


#  imputing ethnicity data 

person_frame<-testing[is.na(lsoa11)==F&lsoa11!="", list(age=min(age, na.rm = T), sex=min(as.numeric(as.factor(sex)),na.rm = T)), by=.(pid,eth_group,lsoa11) ]

# source(scripts/lsoa_risk_compiler.R)
load(normalizePath("./other_data/lsoa_risk.RData"))

md.pattern(ag_risk_lsoa[, c("imd_score","prop_black","prop_asian","prop_mixedother","prop_students","ch_prop"),]) # missing imd data for wales
#  combine with lsoa data 

person_frame=merge(person_frame,ag_risk_lsoa[, .(lsoa11,imd_score,prop_black,prop_asian,prop_mixedother,prop_students,ch_prop) ], by="lsoa11", all.x.y=T)

person_frame[, eth_group:=as.factor(eth_group)]

# not sure why there are some missing lsoa level variables 
person_frame<-person_frame[,c("eth_group","age", "sex", "imd_score","prop_black","prop_asian","prop_mixedother","prop_students","ch_prop", "lsoa11","pid")]

md.pattern(person_frame)
new.pred <- quickpred(person_frame) # To avoid overfitting
imputed_Data <- mice(person_frame, m=1, maxit = 1, seed = 500, pred= new.pred )

imp_data1<- as.data.table(complete(imputed_Data,1))


imp_data1<-imp_data1[, .(pid,eth_group_imp=eth_group)]

testing<-merge(testing, imp_data1, by="pid", all.x=T)

with(testing, prop.table(table(eth_group_imp, useNA = "ifany")))


save(testing,file=normalizePath("./other_data/clean_testing.Rdata"))

