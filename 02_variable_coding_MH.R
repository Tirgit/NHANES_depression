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

full_df$DPQ100 <- NULL


saveRDS(full_df, "cleaned_full_df.rds")

# save data per survey - this is how we will impute missing data
for (i in levels(full_df$survey_nr)) {
  
  filename <- paste0("full_",i,".rds")
  survey_df <- full_df[full_df$survey_nr == i,]
  saveRDS(survey_df, filename)
  
}

