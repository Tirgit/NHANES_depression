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


######################################
########## NHANES 2005-2006 ##########
######################################

years <- "2005-2006"
letter <- "D"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHINC", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150")
occ <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160D", "MCQ160F")
medical <- foreign::read.xport(tf)

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "DIQ010")
diabetes <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2005_2006 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN")  %>%
  full_join(diabetes,  by = "SEQN")
  

## RENAME VARIABLES
colnames(full_2005_2006) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", "marital", 
                              "citizen", "household_size", "family_PIR", 
                              "work_status",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090", "DPQ100",
                              "asthma", "arthritis", "angina", "stroke",
                              "diabetes")

rm(demo, occ, depression)

######################################
########## NHANES 2007-2008 ##########
######################################

years <- "2007-2008"
letter <- "E"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150")
occ <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160D", "MCQ160F")
medical <- foreign::read.xport(tf)

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "DIQ010")
diabetes <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2007_2008 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN")
  

## RENAME VARIABLES
colnames(full_2007_2008) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", "marital", 
                              "citizen", "household_size", "family_PIR", 
                              "work_status",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090", "DPQ100",
                              "asthma", "arthritis", "angina", "stroke",
                              "diabetes")

rm(demo, occ, depression)

######################################
########## NHANES 2009-2010 ##########
######################################
years <- "2009-2010"
letter <- "F"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150")
occ <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "MCQ010", "MCQ160A", "MCQ160D", "MCQ160F")
medical <- foreign::read.xport(tf)

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "DIQ010")
diabetes <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2009_2010 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN")

## RENAME VARIABLES
colnames(full_2009_2010) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090", "DPQ100",
                              "asthma", "arthritis", "angina", "stroke",
                              "diabetes")

rm(demo, occ, depression)


######################################
########## NHANES 2011-2012 ##########
######################################
years <- "2011-2012"
letter <- "G"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150")
occ <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "MCQ010", "MCQ160a", "MCQ160d", "MCQ160f")
medical <- foreign::read.xport(tf)

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "DIQ010")
diabetes <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2011_2012 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN") 

## RENAME VARIABLES
colnames(full_2011_2012) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090", "DPQ100",
                              "asthma", "arthritis", "angina", "stroke",
                              "diabetes")

rm(demo, occ, depression)

######################################
########## NHANES 2013-2014 ##########
######################################
years <- "2013-2014"
letter <- "H"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150")
occ <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "MCQ010", "MCQ160a", "MCQ160d", "MCQ160f")
medical <- foreign::read.xport(tf)

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "DIQ010")
diabetes <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2013_2014 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN")

## RENAME VARIABLES
colnames(full_2013_2014) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090", "DPQ100",
                              "asthma", "arthritis", "angina", "stroke",
                              "diabetes")

rm(demo, occ, depression)

######################################
########## NHANES 2015-2016 ##########
######################################
years <- "2015-2016"
letter <- "I"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150")
occ <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "MCQ010", "MCQ160a", "MCQ160d", "MCQ160f")
medical <- foreign::read.xport(tf)

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "DIQ010")
diabetes <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2015_2016 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2015_2016) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090", "DPQ100",
                              "asthma", "arthritis", "angina", "stroke",
                              "diabetes")

rm(demo, occ, depression)

######################################
########## NHANES 2017-2018 ##########
######################################
years <- "2017-2018"
letter <- "J"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")
mcq_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
diab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2", "DMDEDUC3", "INDHHIN2", "DMDMARTL", "DMDCITZN", "DMDHHSIZ", "INDFMPIR")
demo <- loaded_file[,keep_vars]

## OCCUPATION
download.file(occ_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "OCD150")
occ <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)

## MEDICAL QUESTIONNARIE
download.file(mcq_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "MCQ010", "MCQ160a", "MCQ160d", "MCQ160f")
medical <- foreign::read.xport(tf)

## DIABETES
download.file(diab_file, tf <- tempfile(), mode="wb")
keep_vars <- c("SEQN", "DIQ010")
diabetes <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2017_2018 <- demo %>% 
  full_join(occ,  by = "SEQN") %>%
  full_join(depression,  by = "SEQN") %>%
  full_join(medical,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2017_2018) <- c("SEQN","survey_weight", "SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "education_young", "ann_household_income", 
                              "marital", "citizen", "household_size", "family_PIR", 
                              "work_status",
                              "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                              "DPQ060", "DPQ070", "DPQ080", "DPQ090", "DPQ100",
                              "asthma", "arthritis", "angina", "stroke",
                              "diabetes")

rm(demo, occ, depression)


full_df <- rbind(full_2005_2006,
                 full_2007_2008,
                 full_2009_2010,
                 full_2011_2012,
                 full_2013_2014,
                 full_2015_2016,
                 full_2017_2018)

saveRDS(full_df, "full_df.rds")


