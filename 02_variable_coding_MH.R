# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# load necessary libraries
library(plyr)
library(dplyr)

# load data
full_df <- readRDS("full_df.rds")

#################
##### SEQN ######
#################
# serial number, nothing to change

##########################
##### survey_weight ######
##########################
# survey weight for each 2 year data batch
# for 1999-2000 and 2001-2002, it is ALSO 2 year data weight, NOT the 4 year weight
# if data is actually merged, weights need to be recalculated
# but if we are analyzing per batch, weights are ready to be used
summary(full_df$survey_weight)

# removal of all individuals with NA as weight
full_df <- full_df[!is.na(full_df$survey_weight),]
summary(full_df$survey_weight)


######################
##### survey_nr ######
######################
# survey number, refers to data collection batch
# 4 = 2005_2006
# 5 = 2007_2008
# 6 = 2009_2010
# 7 = 2011_2012
# 8 = 2013_2014
# 9 = 2015_2016
# 10 = 2017_2018
table(full_df$survey_nr, useNA = "always")
full_df$survey_nr <- as.factor(full_df$survey_nr)
full_df$survey_nr <- revalue(full_df$survey_nr, c("4"="2005_2006",
                                                  "5"="2007_2008",
                                                  "6"="2009_2010",
                                                  "7"="2011_2012",
                                                  "8"="2013_2014",
                                                  "9"="2015_2016",
                                                  "10"="2017_2018"))
table(full_df$survey_nr, useNA = "always")


###################
##### gender ######
###################
# survey weight for each 2 year data batch
# 1 = male
# 2 = female
table(full_df$gender, useNA = "always")
full_df$gender <- as.factor(full_df$gender)
full_df$gender <- revalue(full_df$gender, c("1"="male", "2"="female"))
table(full_df$gender, useNA = "always")


################
##### age ######
################
# numeric variable, measurement unit: yrs
summary(full_df$age)


######################
##### ethnicity ######
######################
# factor variable, coded as:
# 1 = Mexican American
# 2 = Other Hispanic
# 3 = Non-Hispanic White
# 4 = Non-Hispanic Black
# 5 = Other Race - Including Multi-Racial
table(full_df$ethnicity, useNA = "always")
# create hispanic category by merging 1 and 2
full_df$ethnicity[full_df$ethnicity == 2] <- 1
full_df$ethnicity[full_df$ethnicity == 3] <- 2
full_df$ethnicity[full_df$ethnicity == 4] <- 3
full_df$ethnicity[full_df$ethnicity == 5] <- 4
full_df$ethnicity <- as.factor(full_df$ethnicity)
full_df$ethnicity <- revalue(full_df$ethnicity, c("1"="Hispanic", 
                                                  "2"="White",
                                                  "3"="Black",
                                                  "4"="Other"))
table(full_df$ethnicity, useNA = "always")


######################
##### education ######
######################
# factor variable, coded as:
# 1 = Less Than 9th Grade
# 2 = 9-11th Grade (Includes 12th grade with no diploma)
# 3 = High School Grad/GED or Equivalent
# 4 = Some College or AA degree
# 5 = College Graduate or above
# 7 = Refused
# 9 = Don't Know
# note - there are no level 6 and level 8 (not a mistake)
table(full_df$education, useNA = "always")
# setting those who refused and don't know to missing
full_df$education[full_df$education == 7] <- NA
full_df$education[full_df$education == 9] <- NA
full_df$education <- as.factor(full_df$education)
full_df$education <- revalue(full_df$education, c("1"="<9 grade", 
                                                  "2"="9-11 grade",
                                                  "3"="GED",
                                                  "4"="some college",
                                                  "5"="college"))
table(full_df$education, useNA = "always")


########################################
####### education in <19 yrs old #######
########################################
table(full_df$education_young, useNA = "always")
# setting those who refused and don't know to missing
full_df$education_young[full_df$education_young == 77] <- NA
full_df$education_young[full_df$education_young == 99] <- NA
full_df$education_young <- as.factor(full_df$education_young)
full_df$education_young <- revalue(full_df$education_young, c("0"="never attended",
                                                              "1"="1 grade",
                                                              "2"="2 grade",
                                                              "3"="3 grade",
                                                              "4"="4 grade",
                                                              "5"="5 grade",
                                                              "6"="6 grade",
                                                              "7"="7 grade",
                                                              "8"="8 grade",
                                                              "9"="9 grade",
                                                              "10"="10 grade",
                                                              "11"="11 grade",
                                                              "12"="12 grade",
                                                              "13"="High School Graduate",
                                                              "14"="GED",
                                                              "15"="More than high school",
                                                              "55"="<5 grade",
                                                              "66"="<9 grade"))
table(full_df$education_young, useNA = "always")



######################
####### income #######
######################
table(full_df$ann_household_income, useNA = "always")
# setting those who refused and don't know to missing
full_df$ann_household_income[full_df$ann_household_income == 77] <- NA
full_df$ann_household_income[full_df$ann_household_income == 99] <- NA
full_df$ann_household_income <- as.factor(full_df$ann_household_income)
full_df$ann_household_income <- revalue(full_df$ann_household_income, c("1"="0-4,999", 
                                                  "2"="5,000-9,999",
                                                  "3"="10,000-14,999",
                                                  "4"="15,000-19,999",
                                                  "5"="20,000-24,999",
                                                  "6"="25,000-34,999",
                                                  "7"="35,000-44,999",
                                                  "8"="45,000-54,999",
                                                  "9"="55,000-64,999",
                                                  "10"="65,000-74,999",
                                                  "11"="75,000 and over",
                                                  "12"="20,000 and over",
                                                  "13"="Under 20,000",
                                                  "14"="75,000-99,999",
                                                  "15"="100,000 and over"))
table(full_df$ann_household_income, useNA = "always")


#######################
##### occupation ######
#######################
table(full_df$work_status, useNA = "always")
# setting those who refused and don't know to missing
full_df$work_status[full_df$work_status == 7] <- NA
full_df$work_status[full_df$work_status == 9] <- NA
full_df$work_status <- as.factor(full_df$work_status)
full_df$work_status <- revalue(full_df$work_status, c("1"="Working", 
                                                      "2"="Has a job but not at work",
                                                      "3"="Looking for work",
                                                      "4"="Not working"))
table(full_df$work_status, useNA = "always")


###########################
##### work situation ######
###########################
table(full_df$work_situation, useNA = "always")
# setting those who refused and don't know to missing
full_df$work_situation[full_df$work_situation == 77] <- NA
full_df$work_situation[full_df$work_situation == 99] <- NA
full_df$work_situation <- as.factor(full_df$work_situation)
full_df$work_situation <- revalue(full_df$work_situation, c("1"="employee_private_company", 
                                                      "2"="federal_gov_employee",
                                                      "3"="state_gov_employee",
                                                      "4"="local_gov_employee",
                                                      "5"="self_employed",
                                                      "6"="working_without_pay"))
table(full_df$work_situation, useNA = "always")

###################################
##### reason for not working ######
###################################
table(full_df$reason_not_working, useNA = "always")
# setting those who refused and don't know to missing
full_df$reason_not_working[full_df$reason_not_working == 77] <- NA
full_df$reason_not_working[full_df$reason_not_working == 99] <- NA
full_df$reason_not_working <- as.factor(full_df$reason_not_working)
full_df$reason_not_working <- revalue(full_df$reason_not_working, c("1"="caretaker", 
                                                                    "2"="school",
                                                                    "3"="retired",
                                                                    "4"="health_reasons",
                                                                    "5"="layoff",
                                                                    "6"="disability",
                                                                    "7"="other"))
table(full_df$reason_not_working, useNA = "always")



###########################
##### marital status ######
###########################
table(full_df$marital, useNA = "always")
# setting those who refused and don't know to missing
full_df$marital[full_df$marital == 77] <- NA
full_df$marital[full_df$marital == 99] <- NA
full_df$marital <- as.factor(full_df$marital)
full_df$marital <- revalue(full_df$marital, c("1"="Married",
                                              "2"="Widowed",
                                              "3"="Divorced",
                                              "4"="Separated",
                                              "5"="Never married",
                                              "6"="Living with partner"))
table(full_df$marital, useNA = "always")


###########################
####### citizenship #######
###########################
table(full_df$citizen, useNA = "always")
# setting those who refused and don't know to missing
full_df$citizen[full_df$citizen == 7] <- NA
full_df$citizen[full_df$citizen == 9] <- NA
full_df$citizen <- as.factor(full_df$citizen)
full_df$citizen <- revalue(full_df$citizen, c("1"="Citizen",
                                              "2"="Not citizen"))
table(full_df$citizen, useNA = "always")


######################
####### asthma #######
######################
table(full_df$asthma, useNA = "always")
# setting those who refused and don't know to missing
full_df$asthma[full_df$asthma == 7] <- NA
full_df$asthma[full_df$asthma == 9] <- NA
full_df$asthma <- as.factor(full_df$asthma)
full_df$asthma <- revalue(full_df$asthma, c("1"="Yes",
                                              "2"="No"))
table(full_df$asthma, useNA = "always")


#########################
####### arthritis #######
#########################
table(full_df$arthritis, useNA = "always")
# setting those who refused and don't know to missing
full_df$arthritis[full_df$arthritis == 7] <- NA
full_df$arthritis[full_df$arthritis == 9] <- NA
full_df$arthritis <- as.factor(full_df$arthritis)
full_df$arthritis <- revalue(full_df$arthritis, c("1"="Yes",
                                            "2"="No"))
table(full_df$arthritis, useNA = "always")


########################
######## angina ########
########################
table(full_df$angina, useNA = "always")
# setting those who refused and don't know to missing
full_df$angina[full_df$angina == 7] <- NA
full_df$angina[full_df$angina == 9] <- NA
full_df$angina <- as.factor(full_df$angina)
full_df$angina <- revalue(full_df$angina, c("1"="Yes",
                                                  "2"="No"))
table(full_df$angina, useNA = "always")


########################
######## stroke ########
########################
table(full_df$stroke, useNA = "always")
# setting those who refused and don't know to missing
full_df$stroke[full_df$stroke == 7] <- NA
full_df$stroke[full_df$stroke == 9] <- NA
full_df$stroke <- as.factor(full_df$stroke)
full_df$stroke <- revalue(full_df$stroke, c("1"="Yes",
                                            "2"="No"))
table(full_df$stroke, useNA = "always")

##########################
######## diabetes ########
##########################
table(full_df$diabetes, useNA = "always")
# setting those who refused and don't know to missing
full_df$diabetes[full_df$diabetes == 7] <- NA
full_df$diabetes[full_df$diabetes == 9] <- NA
full_df$diabetes <- as.factor(full_df$diabetes)
full_df$diabetes <- revalue(full_df$diabetes, c("1"="Yes",
                                            "2"="No",
                                            "3"="Borderline"))
table(full_df$diabetes, useNA = "always")


##############################
######## hypertension ########
##############################
table(full_df$hypertension, useNA = "always")
# setting those who refused and don't know to missing
full_df$hypertension[full_df$hypertension == 7] <- NA
full_df$hypertension[full_df$hypertension == 9] <- NA
full_df$hypertension <- as.factor(full_df$hypertension)
full_df$hypertension <- revalue(full_df$hypertension, c("1"="Yes",
                                                "2"="No"))
table(full_df$hypertension, useNA = "always")

#########################
######## smoking ########
#########################
# 1 = Every day
# 2 = Some days
# 3 = Not at all
# 7 = Refused
# 9 = Don't know
table(full_df$smoking, useNA = "always")
full_df$smoking[full_df$smoking == 7] <- NA
full_df$smoking[full_df$smoking == 9] <- NA
# recode so that 1 = smoker, 2 = nonsmoker
full_df$smoking[full_df$smoking == 2] <- 1
table(full_df$smoking, useNA = "always")
full_df$smoking[full_df$smoking == 3] <- 2
full_df$smoking <- as.factor(full_df$smoking)
full_df$smoking <- revalue(full_df$smoking, c("1"="smoker",
                                              "2"="non-smoker"))
table(full_df$smoking, useNA = "always")


#########################
######## alcohol ########
#########################
table(full_df$alcohol, useNA = "always")
# setting those who refused and don't know to missing
full_df$alcohol[full_df$alcohol == 777] <- NA
full_df$alcohol[full_df$alcohol == 999] <- NA
# probably we should categorize
# full_df$alcohol <- as.factor(full_df$alcohol)
# full_df$alcohol <- revalue(full_df$alcohol, c("1"="Yes",
#                                               "2"="No"))
table(full_df$alcohol, useNA = "always")

#############################################
####### family Poverty index category #######
#############################################
table(full_df$family_pov_level, useNA = "always")
# setting those who refused and don't know to missing
full_df$family_pov_level[full_df$family_pov_level == 7] <- NA
full_df$family_pov_level[full_df$family_pov_level == 9] <- NA
full_df$family_pov_level <- as.factor(full_df$family_pov_level)
full_df$family_pov_level <- revalue(full_df$family_pov_level, c("1"="<=1.3",
                                              "2"="1.3-1.85",
                                              "3"=">1.85"))
table(full_df$family_pov_level, useNA = "always")

###########################
####### PA vigorous #######
###########################
summary(full_df$PA_vigorous, useNA = "always")
# setting those who refused and don't know to missing
full_df$PA_vigorous[full_df$PA_vigorous == 7777] <- NA
full_df$PA_vigorous[full_df$PA_vigorous == 9999] <- NA
summary(full_df$PA_vigorous, useNA = "always")

###########################
####### PA moderate #######
###########################
summary(full_df$PA_moderate, useNA = "always")
# setting those who refused and don't know to missing
full_df$PA_moderate[full_df$PA_moderate == 7777] <- NA
full_df$PA_moderate[full_df$PA_moderate == 9999] <- NA
summary(full_df$PA_moderate, useNA = "always")

############################
####### PA sedentary #######
############################
summary(full_df$PA_sedentary, useNA = "always")
# setting those who refused and don't know to missing
full_df$PA_sedentary[full_df$PA_sedentary == 7777] <- NA
full_df$PA_sedentary[full_df$PA_sedentary == 9999] <- NA
summary(full_df$PA_sedentary, useNA = "always")

#################################
####### PA transportation #######
#################################
summary(full_df$PA_transportation, useNA = "always")
# setting those who refused and don't know to missing
full_df$PA_transportation[full_df$PA_transportation == 7777] <- NA
full_df$PA_transportation[full_df$PA_transportation == 9999] <- NA
summary(full_df$PA_transportation, useNA = "always")


##############################
####### household size #######
##############################
table(full_df$household_size, useNA = "always")

####################################################
####### family Poverty to Income Ratio (PIR) #######
####################################################
summary(full_df$family_PIR)

####################################
####### family Poverty index #######
####################################
summary(full_df$family_pov_index)

###################
####### BMI #######
###################
summary(full_df$BMI)

#########################
#### PHQ9 answers ######
#########################
# 0 = No
# 1 = Yes
full_df$phq <- ifelse(full_df$DPQ010<=9 | full_df$DPQ020<=9 | full_df$DPQ030<=9 
                           | full_df$DPQ040<=9 | full_df$DPQ050<=9 | full_df$DPQ060<=9 
                           | full_df$DPQ070<=9 | full_df$DPQ080<=9 | full_df$DPQ090<=9, 1, 0)
full_df$phq[is.na(full_df$phq)] <- 0

table(full_df$phq, useNA = "always")
full_df$phq <- as.factor(full_df$phq)
full_df$phq <- revalue(full_df$phq, c("0"="No", "1"="Yes"))
table(full_df$phq, useNA = "always")


# SAVE FULL FINAL DATASET 
saveRDS(full_df, "cleaned_full_df.rds")

# SAVE DATA PER SURVEY - this is how we will impute missing data
for (i in levels(full_df$survey_nr)) {
  
  filename <- paste0("full_",i,".rds")
  survey_df <- full_df[full_df$survey_nr == i,]
  saveRDS(survey_df, filename)
  
}

