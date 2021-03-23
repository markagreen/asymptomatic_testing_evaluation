### Code for Appendix F ###

# Credit to Dr David Hughes
library(dplyr)

memory.limit(size=56000000000)
#data1<-read.delim("COVIDPILLAR2_VCam_MastP2.txt",header = TRUE,quote="")
data1<-load(normalizePath("./other_data/testing.Rdata"))
##Last two rows seem to have NAs for basically everything - safe to remove I think
#data1<-data1[1:(nrow(data1)-2),]
data1$AppointmentDate<-as.Date(as.character(data1$AppointmentDate))
##order observations by appointment date.
data1<-data1[with(data1,order(PseudoID,AppointmentDate)),]
data1$SpecimenProcessedDate<-as.Date(as.character(data1$SpecimenProcessedDate))

##Only consider dates after 6th November
data1<-data1[data1$AppointmentDate>="2020-11-06",]


data1$TypeofTest<-"LFT"
data1$TypeofTest[data1$TestResult=="SCT:1240581000000104"]<-"PCR"
data1$TypeofTest[data1$TestResult=="SCT:1240591000000102"]<-"PCR"
data1$TypeofTest[data1$TestResult=="SCT:1321691000000102"]<-"PCR"
data1$TypeofTest<-as.factor(data1$TypeofTest)
##Also coded in "TestKit" but this was just to check they agree with the results codes.

##Count number of PCR tests
f1<-function(x){sum(x=="PCR")}
check1<-aggregate(TypeofTest~PseudoID,f1,data=data1)
names(check1)<-c("PseudoID","TotalPCR")
data1<-left_join(data1,check1)

##Count number of LFT tests
f2<-function(x){sum(x=="LFT")}
check2<-aggregate(TypeofTest~PseudoID,f2,data=data1)
names(check2)<-c("PseudoID","TotalLFT")
data1<-left_join(data1,check2)

##Need to code Symptomatic or not... coded in "TestReason" or "CovidSymptomatic"
data1$SYMPTOMS<-"No"
data1$SYMPTOMS[data1$TestReason=="symptomatic-citizen"]<-"Yes"
data1$SYMPTOMS[data1$TestReason=="symptomatic-essential-worker"]<-"Yes"
data1$SYMPTOMS[data1$TestReason=="I have coronavirus symptoms"]<-"Yes"
data1$SYMPTOMS[data1$CovidSymptomatic=="true"]<-"Yes"
data1$SYMPTOMS<-as.factor(data1$SYMPTOMS)

data1$Ch1Cq<-as.numeric(as.character(data1$Ch1Cq))
data1$Ch2Cq<-as.numeric(as.character(data1$Ch2Cq))
data1$Ch3Cq<-as.numeric(as.character(data1$Ch3Cq))
data1$Ch3Cq[data1$Ch3Target=="MS2"]<-NA
f1<-function(x){mean(x,na.rm=TRUE)}
data1$CTRes<-apply(data1[,c("Ch2Cq","Ch3Cq","Ch1Cq")],1,f1)

##Liverpool residents
data1<-subset(data1,data1$LowerTierLocalAuthority=="E08000012")

##First look at % of individuals who have a PCR within 5 days for both positive and negative lft
##Look at positive LFT tests - how many then had a PCR test to confirm. How many agreed?
sub1<-subset(data1,data1$TestResultDesc=="lft positive")
id1<-unique(sub1$PseudoID)
##Select all individuals within main dataset who have at least one positive LFT
sub2<-subset(data1,data1$PseudoID %in% unique(sub1$PseudoID))
sub2<-sub2[,c("PseudoID","AppointmentDate","SYMPTOMS","TestCentreID","TestResultDesc","TypeofTest","Age")]
##12889 individuals with positive LFTs by 25/01/2021

##Set up vectors with 1 row per patient...
HasConfPCR<-vector(length=length(id1))
DateConfPCR<-vector(length=length(id1))
DatePositiveLFT<-vector(length=length(id1))
PCRResult<-vector(length=length(id1))
HasSymptoms<-vector(length=length(id1))
PCRTestCentre<-vector(length=length(id1))
LFTTestCentre<-vector(length=length(id1))
Age<-vector(length=length(id1))

##loop to check if they have a confirmatory PCR, if so, 
for(i in 1:length(id1)){
t1<-subset(sub2,sub2$PseudoID==id1[i])
if(nrow(t1)==1){
   HasConfPCR[i]<-"No"
   DatePositiveLFT[i]<-as.Date(t1$AppointmentDate,format="%Y-%m-%d",origin="1970-01-01")
   HasSymptoms[i]<-t1$SYMPTOMS
   LFTTestCentre[i]<-as.character(t1$TestCentreID)
   Age[i]<-t1$Age}
if(nrow(t1)>1){
s1<-which(t1$TestResultDesc=="lft positive")
s2<-which(t1$TypeofTest=="PCR" & t1$AppointmentDate>=t1$AppointmentDate[s1[1]])
   if(length(s2)==0){
      HasConfPCR[i]<-"No"
      DatePositiveLFT[i]<-t1$AppointmentDate[s1[1]]
      HasSymptoms[i]<-t1$SYMPTOMS[s1[1]]
      LFTTestCentre[i]<-as.character(t1$TestCentreID[s1[1]])
      Age[i]<-t1$Age[1]}
   if(length(s2)>0){
     for(k in 1:length(s1)){
        s3<-which(t1$TypeofTest=="PCR" & (t1$AppointmentDate-t1$AppointmentDate[s1[k]])<=5 & (t1$AppointmentDate-t1$AppointmentDate[s1[k]])>=0)
        if(length(s3)>0& HasConfPCR[i]!="Yes"){ 
        HasConfPCR[i]<-"Yes"
        DateConfPCR[i]<-t1$AppointmentDate[s3[1]]
        DatePositiveLFT[i]<-t1$AppointmentDate[s1[k]]
        PCRResult[i]<-t1$TestResultDesc[s3[1]]
        HasSymptoms[i]<-t1$SYMPTOMS[s1[k]]
        LFTTestCentre[i]<-as.character(t1$TestCentreID[s1[k]])
        PCRTestCentre[i]<-as.character(t1$TestCentreID[s3[1]])
        Age[i]<-t1$Age[1]
        }
        if(length(s3)==0 & HasConfPCR[i]!="Yes"){ 
        HasConfPCR[i]<-"No"
        DatePositiveLFT[i]<-t1$AppointmentDate[s1[k]]
        HasSymptoms[i]<-t1$SYMPTOMS[s1[k]]
        LFTTestCentre[i]<-as.character(t1$TestCentreID[s1[k]])
        Age[i]<-t1$Age[1]}
     }
   }
}
}

newdata<-as.data.frame(cbind(id1 = as.numeric(id1), HasConfPCR = as.character(HasConfPCR), DateConfPCR = DateConfPCR, DatePositiveLFT = DatePositiveLFT, PCRResult = as.character(PCRResult), HasSymptoms = HasSymptoms, LFTTestCentre = as.character(LFTTestCentre), PCRTestCentre = as.character(PCRTestCentre), Age = Age))
newdata$Newdate<-as.numeric(as.character(newdata$DatePositiveLFT))
newdata$LFTDate<-as.Date(newdata$Newdate,format="%Y-%m-%d",origin="1970-01-01")
newdata$Newdate2<-as.numeric(as.character(newdata$DateConfPCR))
newdata$PCRDate<-as.Date(newdata$Newdate2,format="%Y-%m-%d",origin="1970-01-01")
newdata$diff<-as.numeric(newdata$PCRDate-newdata$LFTDate)
check1<-subset(newdata,newdata$HasConfPCR=="Yes")

tmp1<-subset(newdata,newdata$LFTDate<="2020-12-02")
tmp2<-subset(newdata,newdata$LFTDate>="2020-12-03" & newdata$LFTDate<="2021-01-05")
tmp3<-subset(newdata,newdata$LFTDate>="2021-01-06" & newdata$LFTDate<="2021-01-31")

sum(tmp1$HasConfPCR=="Yes")
sum(tmp2$HasConfPCR=="Yes")
sum(tmp3$HasConfPCR=="Yes")

st1<-subset(tmp3,tmp3$HasConfPCR=="Yes" & tmp3$diff==0)
summary(st1$PCRResult)
st2<-subset(tmp3,tmp3$HasConfPCR=="Yes" & tmp3$diff<=1)
summary(st2$PCRResult)


### Check characteristics of people who got a confirmatory PCR (MAG additional analysis) ###

# Libraries
library(data.table)
library(lme4)

# Load data with imputed ethnicity
load(normalizePath("other_data/clean_testing.Rdata")) # Load
testing[order(t_date), num_test:=1:.N, by=.(pid)] # Order
testing<-testing[num_test==1] # Just select first test (as only need age, sex and ethnicity)
testing$pid <- as.numeric(testing$pid) # So match

# Join on key characteristics
newdata <- data.table(newdata)
newdata$id1 <- as.numeric(as.character(newdata$id1))
newdata <- merge(newdata, testing, by.x = "id1", by.y = "pid", all.x = TRUE)

# Join on deprivation data and other covariates
load(normalizePath("other_data/lsoa_risk.RData")) # Load Ben's data
ag_risk_lsoa$prop_students <- ag_risk_lsoa$prop_students / 100 # Variable is percentage, so convert to proportion for consistency
ag_risk_lsoa$ch_prop <- ag_risk_lsoa$ch_prop / 100 # Same for care home beds
# Recode ch_prop as binary (care home in LSOA)
ag_risk_lsoa$ch_binary <- NA
ag_risk_lsoa$ch_binary[ag_risk_lsoa$ch_prop == 0] <- 0
ag_risk_lsoa$ch_binary[ag_risk_lsoa$ch_prop > 0] <- 1
ag_risk_lsoa$grp_label <- as.factor(ag_risk_lsoa$grp_label) # To define as factor
ag_risk_lsoa$grp_label <- relevel(ag_risk_lsoa$grp_label, ref = "e-Veterans") # Set reference group
newdata <- merge(newdata, ag_risk_lsoa, by = "lsoa11", all.x = TRUE) # Join onto main file

## Model 1 - likelihood of having confirmatory PCR ##

# Subset has confirmatory PCR within 1 day
lft_persons <- newdata[newdata$diff <= 1,] # Note those with <0 have not had a PCR
lft_persons$lsoa11[lft_persons$lsoa11 == ""] <- NA # Set as missing

# Select reference groups
lft_persons$eth_group_imp <- as.factor(lft_persons$eth_group_imp)
lft_persons$eth_group_imp <- relevel(lft_persons$eth_group_imp, ref = "white") # Set refs
lft_persons$grp_label <- relevel(lft_persons$grp_label, ref = "e-Veterans") 

# Standardise/centre continuous variables
lft_persons$zimd_score <- scale(lft_persons$imd_score, center = TRUE, scale = TRUE)
lft_persons$zprop_students <- scale(lft_persons$prop_students, center = TRUE, scale = TRUE)
lft_persons$zage <- scale(lft_persons$age, center = TRUE, scale = TRUE)

# Multi level model
lft_persons$outcome <- NA # Create outcome variable
lft_persons$outcome[lft_persons$HasConfPCR == "No"] <- 0
lft_persons$outcome[lft_persons$HasConfPCR == "Yes"] <- 1

# Model
model1 <- glmer(outcome ~ 1 + age + sex + factor(eth_group_imp) + zimd_score + zprop_students + ch_binary + (1|lsoa11), family="binomial", data = lft_persons)
summary(model1)

# Print model results as ORs
cc <- confint(model1,parm="beta_",method="Wald")  ## quick method
ctab <- cbind(est=fixef(model1),cc)
exp(ctab)

## Model 2 - likelihood of PCR positive ##

# Subset has confirmatory PCR within 1 day
lft_conf <- lft_persons[lft_persons$HasConfPCR == "Yes",]

# Multi level model
lft_conf$outcome2 <- NA # Create outcome variable
lft_conf$outcome2[lft_conf$PCRResult == "Negative"] <- 0
lft_conf$outcome2[lft_conf$PCRResult == "Positive"] <- 1
# Model
model2 <- glmer(outcome2 ~ 1 + age + sex + factor(eth_group_imp) + zimd_score + zprop_students + ch_binary + (1|lsoa11), family="binomial", data = lft_conf)
summary(model2)


