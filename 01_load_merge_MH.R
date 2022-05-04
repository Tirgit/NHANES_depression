# Set working directory
setwd("~/GitHub/NHANES_T2D/Data")

# load necessary libraries
library(foreign)
library(dplyr)

# LOAD AND MERGE ALL NECESSARY FILES PER SURVEY
# DPQ only available from 2005
# income is separate file from 2007

######################################
########## NHANES 2005-2006 ##########
######################################

years <- "2005-2006"
letter <- "D"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN")
demo <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)



## MERGE ALL BY SEQN
full_2005_2006 <- demo %>% 
  full_join(body,  by = "SEQN") 

## RENAME VARIABLES
colnames(full_2005_2006) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                              "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2007-2008 ##########
######################################

years <- "2007-2008"
letter <- "E"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")

## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN2")
demo <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)



## MERGE ALL BY SEQN
full_2007_2008 <- demo %>% 
  full_join(body,  by = "SEQN") 

## RENAME VARIABLES
colnames(full_2007_2008) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                              "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2009-2010 ##########
######################################
years <- "2009-2010"
letter <- "F"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN2")
demo <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2009_2010 <- demo %>% 
  full_join(body,  by = "SEQN")

## RENAME VARIABLES
colnames(full_2009_2010) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                              "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)


######################################
########## NHANES 2011-2012 ##########
######################################
years <- "2011-2012"
letter <- "G"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")

## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN4")
demo <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2011_2012 <- demo %>% 
  full_join(body,  by = "SEQN") 

## RENAME VARIABLES
colnames(full_2011_2012) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                              "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2013-2014 ##########
######################################
years <- "2013-2014"
letter <- "H"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN4")
demo <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2013_2014 <- demo %>% 
  full_join(body,  by = "SEQN") 

## RENAME VARIABLES
colnames(full_2013_2014) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                              "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2015-2016 ##########
######################################
years <- "2015-2016"
letter <- "I"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN4")
demo <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2015_2016 <- demo %>% 
  full_join(body,  by = "SEQN") 


## RENAME VARIABLES
colnames(full_2015_2016) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                              "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2017-2018 ##########
######################################
years <- "2017-2018"
letter <- "J"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
income_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/INQ_",letter,".XPT")
housing_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/HOQ_",letter,".XPT")
occ_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/OCQ_",letter,".XPT")
dep_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DPQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN4")
demo <- loaded_file[,keep_vars]

## DEPRESSION
download.file(dep_file, tf <- tempfile(), mode="wb")
depression <- foreign::read.xport(tf)


## MERGE ALL BY SEQN
full_2017_2018 <- demo %>% 
  full_join(body,  by = "SEQN") 


## RENAME VARIABLES
colnames(full_2017_2018) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                              "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                              "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)


full_df <- rbind(full_2005_2006,
                 full_2007_2008,
                 full_2009_2010,
                 full_2011_2012,
                 full_2013_2014,
                 full_2015_2016,
                 full_2017_2018)

saveRDS(full_df, "full_df.rds")


