
# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# load necessary libraries
library(foreign)
library(dplyr)

# LOAD AND MERGE ALL NECESSARY FILES PER SURVEY
# DPQ only available from 2005, in the MEC sample
# depression: eligible participants are the mobile exam center (MEC) participants
# MEC 2-year exam weight is used
# income info is in separate file from 2007 (but annual household income is in the demo file for all years)
# a couple of covariates are missing 2005-2006, so omitting this from the download


######################################
########## NHANES 2005-2006 ##########
######################################

# years <- "2005-2006"
# letter <- "D"
# 
# demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
# occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
# dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
# mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
# diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
# bp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
# bmi_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
# smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
# alcohol_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/ALQ_",letter,".XPT")
# 
# 
# ## DEMOGRAPHICS
# download.file(demo_file, tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
#                "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHINC", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
# demo <- loaded_file[,keep_vars]
# 
# ## OCCUPATION
# download.file(occ_file, tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "OCD150", "OCQ260", "OCQ380")
# occ <- loaded_file[,keep_vars]
# 
# ## DEPRESSION
# download.file(dep_file, tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
#                "DPQ060", "DPQ070", "DPQ080", "DPQ090")
# depression <- loaded_file[,keep_vars]
# 
# ## MEDICAL QUESTIONNARIE
# download.file(mcq_file, tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160D", "MCQ160F")
# medical <- loaded_file[,keep_vars]
# 
# ## DIABETES
# download.file(diab_file, tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "DIQ010")
# diabetes <- loaded_file[,keep_vars]
# 
# ## BLOOD PRESSURE
# download.file(bp_file, tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "BPQ020", "BPQ080")
# bloodpressure <- loaded_file[,keep_vars]
# 
# ## BMI
# download.file(bmi_file, tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "BMXBMI")
# bmi <- loaded_file[,keep_vars]
# 
# ## SMOKING
# download.file(smoking_file, tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "SMQ020", "SMQ040")
# smoking <- loaded_file[,keep_vars]
# 
# ## ALCOHOL
# download.file(alcohol_file, tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "ALQ110", "ALQ130")
# alcohol <- loaded_file[,keep_vars]
# 
# 
# ## MERGE ALL BY SEQN
# full_2005_2006 <- demo %>% 
#   full_join(occ,  by = "SEQN") %>%
#   full_join(depression,  by = "SEQN") %>%
#   full_join(medical,  by = "SEQN") %>%
#   full_join(diabetes,  by = "SEQN") %>%
#   full_join(bloodpressure,  by = "SEQN") %>%
#   full_join(bmi,  by = "SEQN") %>%
#   full_join(smoking,  by = "SEQN") %>%
#   full_join(alcohol,  by = "SEQN")
#   
# 
# ## RENAME VARIABLES
# colnames(full_2005_2006) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
#                               "education", "education_young", "ann_household_income", "marital", 
#                               "citizen", "household_size", "family_PIR", 
#                               "work_status", "work_situation", "reason_not_working",
#                               "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
#                               "DPQ060", "DPQ070", "DPQ080", "DPQ090",
#                               "asthma", "arthritis", "angina", "stroke",
#                               "diabetes", "hypertension", "hyperlipidemia", "BMI", "smoking_ever", "smoking_now", "alcohol_ever", "alcohol_quantity")
# 
# rm(demo_file, occ_file, dep_file, mcq_file, diab_file, bp_file, bmi_file, smoking_file, alcohol_file, loaded_file,
#   demo, occ, depression, medical, diabetes, bloodpressure, bmi, smoking, alcohol)


######################################
########## NHANES 2007-2008 ##########
######################################

years <- "2007-2008"
letter <- "E"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
inc_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/INQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
bp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
bmi_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
alcohol_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/ALQ_",letter,".XPT")
pa_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/PAQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150", "OCQ260", "OCQ380")
occ <- loaded_file[,keep_vars]

## INCOME
download.file(inc_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "INDFMMPI", "INDFMMPC")
income <- loaded_file[,keep_vars]


## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
               "DPQ060", "DPQ070", "DPQ080", "DPQ090")
depression <- loaded_file[,keep_vars]

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", 
               "MCQ160F", "MCQ160K", "MCQ160L", "MCQ160M", "MCQ220")
medical <- loaded_file[,keep_vars]

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010")
diabetes <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ080")
bloodpressure <- loaded_file[,keep_vars]

## BMI
download.file(bmi_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXBMI")
bmi <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ020", "SMQ040")
smoking <- loaded_file[,keep_vars]

## ALCOHOL
download.file(alcohol_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "ALQ110", "ALQ130")
alcohol <- loaded_file[,keep_vars]

## PHYSICAL ACTIVITY
download.file(pa_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PAD660", "PAD645", "PAD675", "PAD680")
physical_activity <- loaded_file[,keep_vars]



## MERGE ALL BY SEQN
full_2007_2008 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(income,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN") %>%
  full_join(bloodpressure,  by = "SEQN") %>%
  full_join(bmi,  by = "SEQN") %>%
  full_join(smoking,  by = "SEQN") %>%
  full_join(alcohol,  by = "SEQN") %>%
  full_join(physical_activity,  by = "SEQN")

  

## RENAME VARIABLES
colnames(full_2007_2008) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", "marital", 
                              "citizen", "household_size", "family_PIR", 
                              "work_status", "work_situation", "reason_not_working", "family_pov_index", "family_pov_level",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090",
                              "asthma", "arthritis", "heart_failure", "CHD",
                              "angina", "heart_attack", "stroke", "bronchitis", "liver_condition",
                              "thyroid_condition", "cancer",
                              "diabetes", "hypertension", "hyperlipidemia", "BMI", "smoking_ever", "smoking_now", "alcohol_ever", "alcohol_quantity",
                              "PA_vigorous", "PA_transportation", "PA_moderate", "PA_sedentary")


rm(demo_file, occ_file, dep_file, mcq_file, diab_file, bp_file, bmi_file, smoking_file, alcohol_file, loaded_file,
   demo, occ, depression, medical, diabetes, bloodpressure, bmi, smoking, alcohol, income, inc_file, pa_file, physical_activity)

######################################
########## NHANES 2009-2010 ##########
######################################
years <- "2009-2010"
letter <- "F"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
inc_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/INQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
bp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
bmi_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
alcohol_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/ALQ_",letter,".XPT")
pa_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/PAQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150", "OCQ260", "OCQ380")
occ <- loaded_file[,keep_vars]

## INCOME
download.file(inc_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "INDFMMPI", "INDFMMPC")
income <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
               "DPQ060", "DPQ070", "DPQ080", "DPQ090")
depression <- loaded_file[,keep_vars]

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", 
               "MCQ160F", "MCQ160K", "MCQ160L", "MCQ160M", "MCQ220")
medical <- loaded_file[,keep_vars]

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010")
diabetes <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ080")
bloodpressure <- loaded_file[,keep_vars]

## BMI
download.file(bmi_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXBMI")
bmi <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ020", "SMQ040")
smoking <- loaded_file[,keep_vars]

## ALCOHOL
download.file(alcohol_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "ALQ110", "ALQ130")
alcohol <- loaded_file[,keep_vars]

## PHYSICAL ACTIVITY
download.file(pa_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PAD660", "PAD645", "PAD675", "PAD680")
physical_activity <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2009_2010 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(income,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN") %>%
  full_join(bloodpressure,  by = "SEQN") %>%
  full_join(bmi,  by = "SEQN") %>%
  full_join(smoking,  by = "SEQN") %>%
  full_join(alcohol,  by = "SEQN") %>%
  full_join(physical_activity,  by = "SEQN")

## RENAME VARIABLES
colnames(full_2009_2010) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status", "work_situation", "reason_not_working", "family_pov_index", "family_pov_level",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090",
                              "asthma", "arthritis", "heart_failure", "CHD",
                              "angina", "heart_attack", "stroke", "bronchitis", "liver_condition",
                              "thyroid_condition", "cancer",
                              "diabetes", "hypertension", "hyperlipidemia", "BMI", "smoking_ever", "smoking_now", "alcohol_ever", "alcohol_quantity",
                              "PA_vigorous", "PA_transportation", "PA_moderate", "PA_sedentary")

rm(demo_file, occ_file, dep_file, mcq_file, diab_file, bp_file, bmi_file, smoking_file, alcohol_file, loaded_file,
   demo, occ, depression, medical, diabetes, bloodpressure, bmi, smoking, alcohol, income, inc_file, pa_file, physical_activity)


######################################
########## NHANES 2011-2012 ##########
######################################
years <- "2011-2012"
letter <- "G"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
inc_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/INQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
bp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
bmi_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
alcohol_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/ALQ_",letter,".XPT")
pa_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/PAQ_",letter,".XPT")



## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150", "OCQ260", "OCQ380")
occ <- loaded_file[,keep_vars]

## INCOME
download.file(inc_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "INDFMMPI", "INDFMMPC")
income <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
               "DPQ060", "DPQ070", "DPQ080", "DPQ090")
depression <- loaded_file[,keep_vars]

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", 
               "MCQ160F", "MCQ160K", "MCQ160L", "MCQ160M", "MCQ220")
medical <- loaded_file[,keep_vars]

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010")
diabetes <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ080")
bloodpressure <- loaded_file[,keep_vars]

## BMI
download.file(bmi_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXBMI")
bmi <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ020", "SMQ040")
smoking <- loaded_file[,keep_vars]

## ALCOHOL
download.file(alcohol_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "ALQ110", "ALQ130")
alcohol <- loaded_file[,keep_vars]

## PHYSICAL ACTIVITY
download.file(pa_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PAD660", "PAD645", "PAD675", "PAD680")
physical_activity <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2011_2012 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(income,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN") %>%
  full_join(bloodpressure,  by = "SEQN") %>%
  full_join(bmi,  by = "SEQN") %>%
  full_join(smoking,  by = "SEQN") %>%
  full_join(alcohol,  by = "SEQN") %>%
  full_join(physical_activity,  by = "SEQN")

## RENAME VARIABLES
colnames(full_2011_2012) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status", "work_situation", "reason_not_working", "family_pov_index", "family_pov_level",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090",
                              "asthma", "arthritis", "heart_failure", "CHD",
                              "angina", "heart_attack", "stroke", "bronchitis", "liver_condition",
                              "thyroid_condition", "cancer",
                              "diabetes", "hypertension", "hyperlipidemia", "BMI", "smoking_ever", "smoking_now", "alcohol_ever", "alcohol_quantity",
                              "PA_vigorous", "PA_transportation", "PA_moderate", "PA_sedentary")

rm(demo_file, occ_file, dep_file, mcq_file, diab_file, bp_file, bmi_file, smoking_file, alcohol_file, loaded_file,
   demo, occ, depression, medical, diabetes, bloodpressure, bmi, smoking, alcohol, income, inc_file, pa_file, physical_activity)

######################################
########## NHANES 2013-2014 ##########
######################################
years <- "2013-2014"
letter <- "H"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
inc_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/INQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
bp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
bmi_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
alcohol_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/ALQ_",letter,".XPT")
pa_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/PAQ_",letter,".XPT")



## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150", "OCQ260", "OCQ380")
occ <- loaded_file[,keep_vars]

## INCOME
download.file(inc_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "INDFMMPI", "INDFMMPC")
income <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
               "DPQ060", "DPQ070", "DPQ080", "DPQ090")
depression <- loaded_file[,keep_vars]

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", 
               "MCQ160F", "MCQ160K", "MCQ160L", "MCQ160M", "MCQ220")
medical <- loaded_file[,keep_vars]

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010")
diabetes <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ080")
bloodpressure <- loaded_file[,keep_vars]

## BMI
download.file(bmi_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXBMI")
bmi <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ020", "SMQ040")
smoking <- loaded_file[,keep_vars]

## ALCOHOL
download.file(alcohol_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "ALQ110", "ALQ130")
alcohol <- loaded_file[,keep_vars]

## PHYSICAL ACTIVITY
download.file(pa_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PAD660", "PAD645", "PAD675", "PAD680")
physical_activity <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2013_2014 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(income,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN") %>%
  full_join(bloodpressure,  by = "SEQN") %>%
  full_join(bmi,  by = "SEQN") %>%
  full_join(smoking,  by = "SEQN") %>%
  full_join(alcohol,  by = "SEQN") %>%
  full_join(physical_activity,  by = "SEQN")

## RENAME VARIABLES
colnames(full_2013_2014) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status", "work_situation", "reason_not_working", "family_pov_index", "family_pov_level",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090",
                              "asthma", "arthritis", "heart_failure", "CHD",
                              "angina", "heart_attack", "stroke", "bronchitis", "liver_condition",
                              "thyroid_condition", "cancer",
                              "diabetes", "hypertension", "hyperlipidemia", "BMI", "smoking_ever", "smoking_now", "alcohol_ever", "alcohol_quantity",
                              "PA_vigorous", "PA_transportation", "PA_moderate", "PA_sedentary")

rm(demo_file, occ_file, dep_file, mcq_file, diab_file, bp_file, bmi_file, smoking_file, alcohol_file, loaded_file,
   demo, occ, depression, medical, diabetes, bloodpressure, bmi, smoking, alcohol, income, inc_file, pa_file, physical_activity)

######################################
########## NHANES 2015-2016 ##########
######################################
years <- "2015-2016"
letter <- "I"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
inc_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/INQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
bp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
bmi_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
alcohol_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/ALQ_",letter,".XPT")
pa_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/PAQ_",letter,".XPT")



## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150", "OCQ260", "OCQ380")
occ <- loaded_file[,keep_vars]

## INCOME
download.file(inc_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "INDFMMPI", "INDFMMPC")
income <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
               "DPQ060", "DPQ070", "DPQ080", "DPQ090")
depression <- loaded_file[,keep_vars]

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", 
               "MCQ160F", "MCQ160K", "MCQ160L", "MCQ160M", "MCQ220")
medical <- loaded_file[,keep_vars]

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010")
diabetes <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ080")
bloodpressure <- loaded_file[,keep_vars]

## BMI
download.file(bmi_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXBMI")
bmi <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ020", "SMQ040")
smoking <- loaded_file[,keep_vars]

## ALCOHOL
download.file(alcohol_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "ALQ110", "ALQ130")
alcohol <- loaded_file[,keep_vars]

## PHYSICAL ACTIVITY
download.file(pa_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PAD660", "PAD645", "PAD675", "PAD680")
physical_activity <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2015_2016 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(income,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN") %>%
  full_join(bloodpressure,  by = "SEQN") %>%
  full_join(bmi,  by = "SEQN") %>%
  full_join(smoking,  by = "SEQN") %>%
  full_join(alcohol,  by = "SEQN") %>%
  full_join(physical_activity,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2015_2016) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status", "work_situation", "reason_not_working", "family_pov_index", "family_pov_level",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090",
                              "asthma", "arthritis", "heart_failure", "CHD",
                              "angina", "heart_attack", "stroke", "bronchitis", "liver_condition",
                              "thyroid_condition", "cancer",
                              "diabetes", "hypertension", "hyperlipidemia", "BMI", "smoking_ever", "smoking_now", "alcohol_ever", "alcohol_quantity",
                              "PA_vigorous", "PA_transportation", "PA_moderate", "PA_sedentary")

rm(demo_file, occ_file, dep_file, mcq_file, diab_file, bp_file, bmi_file, smoking_file, alcohol_file, loaded_file,
   demo, occ, depression, medical, diabetes, bloodpressure, bmi, smoking, alcohol, income, inc_file, pa_file, physical_activity)

######################################
########## NHANES 2017-2018 ##########
######################################
years <- "2017-2018"
letter <- "J"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
inc_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/INQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
bp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
bmi_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
alcohol_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/ALQ_",letter,".XPT")
pa_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/PAQ_",letter,".XPT")



## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150", "OCQ260", "OCQ380")
occ <- loaded_file[,keep_vars]

## INCOME
download.file(inc_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "INDFMMPI", "INDFMMPC")
income <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
               "DPQ060", "DPQ070", "DPQ080", "DPQ090")
depression <- loaded_file[,keep_vars]

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", 
               "MCQ160F", "MCQ160K", "MCQ160L", "MCQ160M", "MCQ220")
medical <- loaded_file[,keep_vars]

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010")
diabetes <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ080")
bloodpressure <- loaded_file[,keep_vars]

## BMI
download.file(bmi_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXBMI")
bmi <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ020", "SMQ040")
smoking <- loaded_file[,keep_vars]

## ALCOHOL
download.file(alcohol_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "ALQ111", "ALQ130")
alcohol <- loaded_file[,keep_vars]

## PHYSICAL ACTIVITY
download.file(pa_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PAD660", "PAD645", "PAD675", "PAD680")
physical_activity <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2017_2018 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(income,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN") %>%
  full_join(bloodpressure,  by = "SEQN") %>%
  full_join(bmi,  by = "SEQN") %>%
  full_join(smoking,  by = "SEQN") %>%
  full_join(alcohol,  by = "SEQN") %>%
  full_join(physical_activity,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2017_2018) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status", "work_situation", "reason_not_working", "family_pov_index", "family_pov_level",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090",
                              "asthma", "arthritis", "heart_failure", "CHD",
                              "angina", "heart_attack", "stroke", "bronchitis", "liver_condition",
                              "thyroid_condition", "cancer",
                              "diabetes", "hypertension", "hyperlipidemia", "BMI", "smoking_ever", "smoking_now", "alcohol_ever", "alcohol_quantity",
                              "PA_vigorous", "PA_transportation", "PA_moderate", "PA_sedentary")

rm(demo_file, occ_file, dep_file, mcq_file, diab_file, bp_file, bmi_file, smoking_file, alcohol_file, loaded_file,
   demo, occ, depression, medical, diabetes, bloodpressure, bmi, smoking, alcohol, income, inc_file, pa_file, physical_activity)


full_df <- rbind(full_2007_2008,
                 full_2009_2010,
                 full_2011_2012,
                 full_2013_2014,
                 full_2015_2016,
                 full_2017_2018)

saveRDS(full_df, "full_df.rds")


