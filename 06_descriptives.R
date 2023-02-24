# Set working directory
setwd("~/GitHub/NHANES_depression/Data")

# load libraries
# Load needed libraries
library(tidyverse)
library(survey)
library(tableone)

# Create Rubin's Rules functions
# Rubin's Rules - pooling means
rubin_mean <- function(average) {
  Reduce("+", average) / length(average)
}

# Rubin's Rules - pooling SEs
rubin_se <- function(average, standard_error) {
  # Within imputation variance:
  within_var <- Reduce("+", lapply(standard_error, function(i){i*i})) / length(standard_error)
  # Between imputation variance:
  between_var <- Reduce("+", lapply(average, function(i){(i-rubin_mean(average))*(i-rubin_mean(average))})) / (length(average)-1)
  between_var2 <- between_var/ length(average)
  # Total variance:
  total_var <- within_var+between_var+between_var2
  # Pooled SE:
  sqrt(total_var)
}

# result collection dataframe
means_list <- list()
ses_list <- list()
cat_list <- list()


# DESCRIPTIVES OVERALL

for (i in 1:15) {
  filename <- paste0("cluster_merged_",i,".rds")
  df <- readRDS(filename)
  
  # survey design
  nhanes.y <- svydesign(data=df, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
  
  # subsetting data
  sub.y <- subset(nhanes.y, (df$depressed == 1))
  
  # table creation
  k <- tableone::svyCreateTableOne(vars = c('age', "BMI", "PA_sedentary",
                                            "PA_moderate", "PA_vigorous",
                                            "DPQ_total",
                                            'gender', "marital", 
                                            "education", "ethnicity",
                                            "ann_household_income",
                                            "family_pov_level",
                                            "asthma", "arthritis",
                                            "heart_failure", "CHD",
                                            "angina", "heart_attack", 
                                            "stroke", "bronchitis",
                                            "liver_condition", "thyroid_condition",
                                            "cancer",
                                            "diabetes", "hypertension",
                                            "hyperlipidemia" ,
                                            "smoking", "alcohol",
                                            "dep_category"),
                                   data = sub.y,
                                   includeNA = T,
                                   test = F, addOverall = F)
  
  res_means <- as.matrix(k$ContTable[[1]][,4])
  res_ses <- as.matrix(k$ContTable[[1]][,5])/sqrt(nrow(sub.y))
  means_list[[i]] <- res_means
  ses_list[[i]] <- res_ses
  cat_list[[i]] <- c(
    k$CatTable[[1]][[1]][,6],
    k$CatTable[[1]][[2]][,6],
    k$CatTable[[1]][[3]][,6],
    k$CatTable[[1]][[4]][,6],
    k$CatTable[[1]][[5]][,6],
    k$CatTable[[1]][[6]][,6],
    k$CatTable[[1]][[7]][,6],
    k$CatTable[[1]][[8]][,6],
    k$CatTable[[1]][[9]][,6],
    k$CatTable[[1]][[10]][,6],
    k$CatTable[[1]][[11]][,6],
    k$CatTable[[1]][[12]][,6],
    k$CatTable[[1]][[13]][,6],
    k$CatTable[[1]][[14]][,6],
    k$CatTable[[1]][[15]][,6],
    k$CatTable[[1]][[16]][,6],
    k$CatTable[[1]][[17]][,6],
    k$CatTable[[1]][[18]][,6],
    k$CatTable[[1]][[19]][,6],
    k$CatTable[[1]][[20]][,6],
    k$CatTable[[1]][[21]][,6],
    k$CatTable[[1]][[22]][,6],
    k$CatTable[[1]][[23]][,6])
  
}

means_survey <- rubin_mean(average = means_list)
SDs_survey <- rubin_se(average = means_list, standard_error = ses_list)*sqrt(nrow(sub.y))
cat_survey <- rubin_mean(average = cat_list)

round(means_survey,1)
round(SDs_survey,1)
round(cat_survey,1)[2] # sex: female
round(cat_survey,1)[3:8] # marital
round(cat_survey,1)[9:13] # education
round(cat_survey,1)[14:17] # ethnicity
round(cat_survey,1)[18:20] # income
round(cat_survey,1)[21:23] # family POV
round(cat_survey,1)[24] # asthma: yes
round(cat_survey,1)[26] # arthritis : yes
round(cat_survey,1)[28] # heart failure : yes
round(cat_survey,1)[30] # CHD : yes
round(cat_survey,1)[32] # angina : yes
round(cat_survey,1)[34] # heart attack  : yes
round(cat_survey,1)[36] # stroke  : yes
round(cat_survey,1)[38] # bronchitis  : yes
round(cat_survey,1)[40] # liver_condition  : yes
round(cat_survey,1)[42] # thyroid_condition  : yes
round(cat_survey,1)[44] # cancer  : yes
round(cat_survey,1)[46:48] # diabetes : yes, no, borderline
round(cat_survey,1)[49] # hypertension  : yes
round(cat_survey,1)[51] # hypercholesterolemia  : yes
round(cat_survey,1)[53:56] # smoking  : never, ex, sometimes, current
round(cat_survey,1)[57:59] # alcohol : never, light, heavy
round(cat_survey,1)[60:62] # depression category: mod, mod-sev, sev


# DESCRIPTIVES PER CLUSTER

# result collection dataframe
means_list <- list()
ses_list <- list()
cat_list <- list()


# loading data
for (i in 1:15) {
  filename <- paste0("cluster_merged_",i,".rds")
  df <- readRDS(filename)
  
  # survey design
  nhanes.y <- svydesign(data=df, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
  
  # subsetting data
  sub.y <- subset(nhanes.y, (df$cluster_membership == "1"))
  
  # table creation
  k <- tableone::svyCreateTableOne(vars = c('age', "BMI", "PA_sedentary",
                                            "PA_moderate", "PA_vigorous",
                                            "DPQ_total",
                                            'gender', "marital", 
                                            "education", "ethnicity",
                                            "ann_household_income",
                                            "family_pov_level",
                                            "asthma", "arthritis",
                                            "heart_failure", "CHD",
                                            "angina", "heart_attack", 
                                            "stroke", "bronchitis",
                                            "liver_condition", "thyroid_condition",
                                            "cancer",
                                            "diabetes", "hypertension",
                                            "hyperlipidemia" ,
                                            "smoking", "alcohol",
                                            "dep_category"),
                                   data = sub.y,
                                   includeNA = T,
                                   test = F, addOverall = F)
  
  res_means <- as.matrix(k$ContTable[[1]][,4])
  res_ses <- as.matrix(k$ContTable[[1]][,5])/sqrt(nrow(sub.y))
  means_list[[i]] <- res_means
  ses_list[[i]] <- res_ses
  cat_list[[i]] <- c(
    k$CatTable[[1]][[1]][,6],
    k$CatTable[[1]][[2]][,6],
    k$CatTable[[1]][[3]][,6],
    k$CatTable[[1]][[4]][,6],
    k$CatTable[[1]][[5]][,6],
    k$CatTable[[1]][[6]][,6],
    k$CatTable[[1]][[7]][,6],
    k$CatTable[[1]][[8]][,6],
    k$CatTable[[1]][[9]][,6],
    k$CatTable[[1]][[10]][,6],
    k$CatTable[[1]][[11]][,6],
    k$CatTable[[1]][[12]][,6],
    k$CatTable[[1]][[13]][,6],
    k$CatTable[[1]][[14]][,6],
    k$CatTable[[1]][[15]][,6],
    k$CatTable[[1]][[16]][,6],
    k$CatTable[[1]][[17]][,6],
    k$CatTable[[1]][[18]][,6],
    k$CatTable[[1]][[19]][,6],
    k$CatTable[[1]][[20]][,6],
    k$CatTable[[1]][[21]][,6],
    k$CatTable[[1]][[22]][,6],
    k$CatTable[[1]][[23]][,6])
  
}

means_survey <- rubin_mean(average = means_list)
SDs_survey <- rubin_se(average = means_list, standard_error = ses_list)*sqrt(nrow(sub.y))
cat_survey <- rubin_mean(average = cat_list)

round(means_survey,1)
round(SDs_survey,1)
round(cat_survey,1)[2] # sex: female
round(cat_survey,1)[3:8] # marital
round(cat_survey,1)[9:13] # education
round(cat_survey,1)[14:17] # ethnicity
round(cat_survey,1)[18:20] # income
round(cat_survey,1)[21:23] # family POV
round(cat_survey,1)[24] # asthma: yes
round(cat_survey,1)[26] # arthritis : yes
round(cat_survey,1)[28] # heart failure : yes
round(cat_survey,1)[30] # CHD : yes
round(cat_survey,1)[32] # angina : yes
round(cat_survey,1)[34] # heart attack  : yes
round(cat_survey,1)[36] # stroke  : yes
round(cat_survey,1)[38] # bronchitis  : yes
round(cat_survey,1)[40] # liver_condition  : yes
round(cat_survey,1)[42] # thyroid_condition  : yes
round(cat_survey,1)[44] # cancer  : yes
round(cat_survey,1)[46:48] # diabetes : yes, no, borderline
round(cat_survey,1)[49] # hypertension  : yes
round(cat_survey,1)[51] # hypercholesterolemia  : yes
round(cat_survey,1)[53:56] # smoking  : never, ex, sometimes, current
round(cat_survey,1)[57:59] # alcohol : never, light, heavy
round(cat_survey,1)[60:62] # depression category: mod, mod-sev, sev




# COMPARING NUMERIC PER CLUSTER

# P value collection
p_list <- list()

# 'age', "BMI", "PA_sedentary", "PA_moderate", "PA_vigorous", "DPQ_total"

for (i in 1:15) {
  filename <- paste0("cluster_merged_",i,".rds")
  df <- readRDS(filename)
  
  # survey design
  nhanes.y <- svydesign(data=df, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
  
  # subsetting data
  sub.y <- subset(nhanes.y, (df$depressed == 1))
  
  # ANOVA
  survey_glm <- svyglm(age ~ cluster_membership, sub.y)
  anova_res <- aov(survey_glm)
  p_list[[i]] <- summary(anova_res)[[1]]$'Pr(>F)'[[1]]

}

median(unlist(p_list))



# COMPARING CATEGORICAL PER CLUSTER

# P value collection
p_list <- list()

# 'gender', "marital", 
# "education", "ethnicity",
# "ann_household_income",
# "family_pov_level",
# "asthma", "arthritis",
# "heart_failure", "CHD",
# "angina", "heart_attack", 
# "stroke", "bronchitis",
# "liver_condition", "thyroid_condition",
# "cancer",
# "diabetes", "hypertension",
# "hyperlipidemia" ,
# "smoking", "alcohol",
# "dep_category"

for (i in 1:15) {
  filename <- paste0("cluster_merged_",i,".rds")
  df <- readRDS(filename)
  
  # survey design
  nhanes.y <- svydesign(data=df, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
  
  # subsetting data
  sub.y <- subset(nhanes.y, (df$depressed == 1))
  
  # chi squared
  tbl <- svychisq(~dep_category + cluster_membership, sub.y)
  p_list[[i]] <- as.numeric(tbl$p.value)

}

median(unlist(p_list))

